using System;
using Microsoft.Xna.Framework;
using Microsoft.Xna.Framework.Graphics;

namespace InstancedModel {
  /// <summary>
  /// Enum describes the various possible techniques
  /// that can be chosen to implement instancing.
  /// </summary>
  public enum InstancingTechnique {
    HardwareInstancing,
    NoInstancing,
  }


  /// <summary>
  /// Sample showing how to efficiently render many copies of a model, using
  /// hardware instancing to draw more than one copy in a single GPU batch.
  /// </summary>
  public class InstancedModelRenderer {
    #region Fields
    GraphicsDeviceManager graphics;

    // Instanced model rendering.
    InstancingTechnique instancingTechnique = InstancingTechnique.HardwareInstancing;

    const int InitialInstanceCount = 1000;

    Matrix[] instanceTransforms;
    Model instancedModel;
    Matrix[] instancedModelBones;
    DynamicVertexBuffer instanceVertexBuffer;

    // To store instance transform matrices in a vertex buffer, we use this custom
    // vertex type which encodes 4x4 matrices as a set of four Vector4 values.
    static VertexDeclaration instanceVertexDeclaration = new VertexDeclaration
    (
        new VertexElement(0, VertexElementFormat.Vector4, VertexElementUsage.BlendWeight, 0),
        new VertexElement(16, VertexElementFormat.Vector4, VertexElementUsage.BlendWeight, 1),
        new VertexElement(32, VertexElementFormat.Vector4, VertexElementUsage.BlendWeight, 2),
        new VertexElement(48, VertexElementFormat.Vector4, VertexElementUsage.BlendWeight, 3)
    );

    #endregion

    #region Initialization

    public InstancedModelRenderer(GraphicsDeviceManager graphics, Model instancedModel) {
      this.graphics = graphics;
      this.instancedModel = instancedModel;
      instancedModelBones = new Matrix[instancedModel.Bones.Count];
      instancedModel.CopyAbsoluteBoneTransformsTo(instancedModelBones);
    }

    #endregion

    #region Update and Draw

    /// <summary>
    /// This is called when the game should draw itself.
    /// </summary>
    public void Draw(Matrix[] transforms, Matrix view, Matrix projection) {
      GraphicsDevice device = graphics.GraphicsDevice;

      // Set renderstates for drawing 3D models.
      device.BlendState = BlendState.Opaque;
      device.DepthStencilState = DepthStencilState.Default;

      // Gather instance transform matrices into a single array.
      Array.Resize(ref instanceTransforms, transforms.Length);

      for (int i = 0; i < transforms.Length; i++) {
        instanceTransforms[i] = transforms[i];
      }

      // Draw all the instances, using the currently selected rendering technique.
      switch (instancingTechnique) {
        case InstancingTechnique.HardwareInstancing:
          DrawModelHardwareInstancing(instancedModel, instancedModelBones,
                                      instanceTransforms, view, projection);
          break;

        case InstancingTechnique.NoInstancing:
          DrawModelNoInstancing(instancedModel, instancedModelBones,
                                instanceTransforms, view, projection);
          break;
      }
    }


    /// <summary>
    /// Efficiently draws several copies of a piece of geometry using hardware instancing.
    /// </summary>
    void DrawModelHardwareInstancing(Model model, Matrix[] modelBones,
                                     Matrix[] instances, Matrix view, Matrix projection) {
      var device = graphics.GraphicsDevice;
      if (instances.Length == 0)
        return;

      // If we have more instances than room in our vertex buffer, grow it to the neccessary size.
      if ((instanceVertexBuffer == null) ||
          (instances.Length > instanceVertexBuffer.VertexCount)) {
        if (instanceVertexBuffer != null)
          instanceVertexBuffer.Dispose();

        instanceVertexBuffer = new DynamicVertexBuffer(device, instanceVertexDeclaration,
                                                       instances.Length, BufferUsage.WriteOnly);
      }

      // Transfer the latest instance transform matrices into the instanceVertexBuffer.
      instanceVertexBuffer.SetData(instances, 0, instances.Length, SetDataOptions.Discard);

      foreach (ModelMesh mesh in model.Meshes) {
        foreach (ModelMeshPart meshPart in mesh.MeshParts) {
          // Tell the GPU to read from both the model vertex buffer plus our instanceVertexBuffer.
          device.SetVertexBuffers(
              new VertexBufferBinding(meshPart.VertexBuffer, meshPart.VertexOffset, 0),
              new VertexBufferBinding(instanceVertexBuffer, 0, 1)
          );

          device.Indices = meshPart.IndexBuffer;

          // Set up the instance rendering effect.
          Effect effect = meshPart.Effect;

          effect.CurrentTechnique = effect.Techniques["HardwareInstancing"];

          effect.Parameters["World"].SetValue(modelBones[mesh.ParentBone.Index]);
          effect.Parameters["View"].SetValue(view);
          effect.Parameters["Projection"].SetValue(projection);

          // Draw all the instance copies in a single call.
          foreach (EffectPass pass in effect.CurrentTechnique.Passes) {
            pass.Apply();

            device.DrawInstancedPrimitives(PrimitiveType.TriangleList, 0, 0,
                                           meshPart.NumVertices, meshPart.StartIndex,
                                           meshPart.PrimitiveCount, instances.Length);
          }
        }
      }
    }


    /// <summary>
    /// Draws several copies of a piece of geometry without using any
    /// special GPU instancing techniques at all. This just does a
    /// regular loop and issues several draw calls one after another.
    /// </summary>
    void DrawModelNoInstancing(Model model, Matrix[] modelBones,
                               Matrix[] instances, Matrix view, Matrix projection) {
      var device = graphics.GraphicsDevice;
      foreach (ModelMesh mesh in model.Meshes) {
        foreach (ModelMeshPart meshPart in mesh.MeshParts) {
          device.SetVertexBuffer(meshPart.VertexBuffer, meshPart.VertexOffset);
          device.Indices = meshPart.IndexBuffer;

          // Set up the rendering effect.
          Effect effect = meshPart.Effect;

          effect.CurrentTechnique = effect.Techniques["NoInstancing"];

          effect.Parameters["View"].SetValue(view);
          effect.Parameters["Projection"].SetValue(projection);

          EffectParameter transformParameter = effect.Parameters["World"];

          // Draw a single instance copy each time around this loop.
          for (int i = 0; i < instances.Length; i++) {
            transformParameter.SetValue(modelBones[mesh.ParentBone.Index] * instances[i]);

            foreach (EffectPass pass in effect.CurrentTechnique.Passes) {
              pass.Apply();

              device.DrawIndexedPrimitives(PrimitiveType.TriangleList, 0, 0,
                                           meshPart.NumVertices, meshPart.StartIndex,
                                           meshPart.PrimitiveCount);
            }
          }
        }
      }
    }
    #endregion
  }
}
