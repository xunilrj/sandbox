std::ostream& operator << (std::ostream& out, VkLayerProperties& layer)
{
    out << layer.layerName << " " 
        << layer.specVersion << " "
        << layer.implementationVersion << " "
        << layer.description << std::endl;
    return out;
}

std::ostream& operator << (std::ostream& out, VkPhysicalDevice& device)
{
    //https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/VkPhysicalDeviceProperties.html
    VkPhysicalDeviceProperties p;
    //https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/vkGetPhysicalDeviceProperties.html
    vkGetPhysicalDeviceProperties(device, &p);

    out << ""
        "apiVersion: " << p.apiVersion << std::endl <<
        "driverVersion: " << p.driverVersion << std::endl <<
        "vendorID: " << p.vendorID << std::endl <<
        "deviceID: " << p.deviceID << std::endl <<
        "deviceType: " << (int)p.deviceType << std::endl <<
        "deviceName: " << p.deviceName << std::endl <<
        "pipelineCacheUUID: " << p.pipelineCacheUUID << std::endl <<
        "limites.maxImageDimension1D: " << p.limits.maxImageDimension1D << std::endl <<
        "limites.maxImageDimension2D: " << p.limits.maxImageDimension2D << std::endl <<
        "limites.maxImageDimension3D: " << p.limits.maxImageDimension3D << std::endl <<
        "limites.maxImageDimensionCube: " << p.limits.maxImageDimensionCube << std::endl <<
        "limites.maxImageArrayLayers: " << p.limits.maxImageArrayLayers << std::endl <<
        "limites.maxTexelBufferElements: " << p.limits.maxTexelBufferElements << std::endl <<
        "limites.maxUniformBufferRange: " << p.limits.maxUniformBufferRange << std::endl <<
        "limites.maxStorageBufferRange: " << p.limits.maxStorageBufferRange << std::endl <<
        "limites.maxPushConstantsSize: " << p.limits.maxPushConstantsSize << std::endl <<
        "limites.maxMemoryAllocationCount: " << p.limits.maxMemoryAllocationCount << std::endl <<
        "limites.maxSamplerAllocationCount: " << p.limits.maxSamplerAllocationCount << std::endl <<
        "limites.bufferImageGranularity: " << p.limits.bufferImageGranularity << std::endl <<
        "limites.sparseAddressSpaceSize: " << p.limits.sparseAddressSpaceSize << std::endl <<
        "limites.maxBoundDescriptorSets: " << p.limits.maxBoundDescriptorSets << std::endl <<
        "limites.maxPerStageDescriptorSamplers: " << p.limits.maxPerStageDescriptorSamplers << std::endl <<
        "limites.maxPerStageDescriptorUniformBuffers: " << p.limits.maxPerStageDescriptorUniformBuffers << std::endl <<
        "limites.maxPerStageDescriptorStorageBuffers: " << p.limits.maxPerStageDescriptorStorageBuffers << std::endl <<
        "limites.maxPerStageDescriptorSampledImages: " << p.limits.maxPerStageDescriptorSampledImages << std::endl <<
        "limites.maxPerStageDescriptorStorageImages: " << p.limits.maxPerStageDescriptorStorageImages << std::endl <<
        "limites.maxPerStageDescriptorInputAttachments: " << p.limits.maxPerStageDescriptorInputAttachments << std::endl <<
        "limites.maxPerStageResources: " << p.limits.maxPerStageResources << std::endl <<
        "limites.maxDescriptorSetSamplers: " << p.limits.maxDescriptorSetSamplers << std::endl <<
        "limites.maxDescriptorSetUniformBuffers: " << p.limits.maxDescriptorSetUniformBuffers << std::endl <<
        "limites.maxDescriptorSetUniformBuffersDynamic: " << p.limits.maxDescriptorSetUniformBuffersDynamic << std::endl <<
        "limites.maxDescriptorSetStorageBuffers: " << p.limits.maxDescriptorSetStorageBuffers << std::endl <<
        "limites.maxDescriptorSetStorageBuffersDynamic: " << p.limits.maxDescriptorSetStorageBuffersDynamic << std::endl <<
        "limites.maxDescriptorSetSampledImages: " << p.limits.maxDescriptorSetSampledImages << std::endl <<
        "limites.maxDescriptorSetStorageImages: " << p.limits.maxDescriptorSetStorageImages << std::endl <<
        "limites.maxDescriptorSetInputAttachments: " << p.limits.maxDescriptorSetInputAttachments << std::endl <<
        "limites.maxVertexInputAttributes: " << p.limits.maxVertexInputAttributes << std::endl <<
        "limites.maxVertexInputBindings: " << p.limits.maxVertexInputBindings << std::endl <<
        "limites.maxVertexInputAttributeOffset: " << p.limits.maxVertexInputAttributeOffset << std::endl <<
        "limites.maxVertexInputBindingStride: " << p.limits.maxVertexInputBindingStride << std::endl <<
        "limites.maxVertexOutputComponents: " << p.limits.maxVertexOutputComponents << std::endl <<
        "limites.maxTessellationGenerationLevel: " << p.limits.maxTessellationGenerationLevel << std::endl <<
        "limites.maxTessellationPatchSize: " << p.limits.maxTessellationPatchSize << std::endl <<
        "limites.maxTessellationControlPerVertexInputComponents: " << p.limits.maxTessellationControlPerVertexInputComponents << std::endl <<
        "limites.maxTessellationControlPerVertexOutputComponents: " << p.limits.maxTessellationControlPerVertexOutputComponents << std::endl <<
        "limites.maxTessellationControlPerPatchOutputComponents: " << p.limits.maxTessellationControlPerPatchOutputComponents << std::endl <<
        "limites.maxTessellationControlTotalOutputComponents: " << p.limits.maxTessellationControlTotalOutputComponents << std::endl <<
        "limites.maxTessellationEvaluationInputComponents: " << p.limits.maxTessellationEvaluationInputComponents << std::endl <<
        "limites.maxTessellationEvaluationOutputComponents: " << p.limits.maxTessellationEvaluationOutputComponents << std::endl <<
        "limites.maxGeometryShaderInvocations: " << p.limits.maxGeometryShaderInvocations << std::endl <<
        "limites.maxGeometryInputComponents: " << p.limits.maxGeometryInputComponents << std::endl <<
        "limites.maxGeometryOutputComponents: " << p.limits.maxGeometryOutputComponents << std::endl <<
        "limites.maxGeometryOutputVertices: " << p.limits.maxGeometryOutputVertices << std::endl <<
        "limites.maxGeometryTotalOutputComponents: " << p.limits.maxGeometryTotalOutputComponents << std::endl <<
        "limites.maxFragmentInputComponents: " << p.limits.maxFragmentInputComponents << std::endl <<
        "limites.maxFragmentOutputAttachments: " << p.limits.maxFragmentOutputAttachments << std::endl <<
        "limites.maxFragmentDualSrcAttachments: " << p.limits.maxFragmentDualSrcAttachments << std::endl <<
        "limites.maxFragmentCombinedOutputResources: " << p.limits.maxFragmentCombinedOutputResources << std::endl <<
        "limites.maxComputeSharedMemorySize: " << p.limits.maxComputeSharedMemorySize << std::endl <<
        "limites.maxComputeWorkGroupCount: " << p.limits.maxComputeWorkGroupCount << std::endl <<
        "limites.maxComputeWorkGroupInvocations: " << p.limits.maxComputeWorkGroupInvocations << std::endl <<
        "limites.maxComputeWorkGroupSize: " << p.limits.maxComputeWorkGroupSize << std::endl <<
        "limites.subPixelPrecisionBits: " << p.limits.subPixelPrecisionBits << std::endl <<
        "limites.subTexelPrecisionBits: " << p.limits.subTexelPrecisionBits << std::endl <<
        "limites.mipmapPrecisionBits: " << p.limits.mipmapPrecisionBits << std::endl <<
        "limites.maxDrawIndexedIndexValue: " << p.limits.maxDrawIndexedIndexValue << std::endl <<
        "limites.maxDrawIndirectCount: " << p.limits.maxDrawIndirectCount << std::endl <<
        "limites.maxSamplerLodBias: " << p.limits.maxSamplerLodBias << std::endl <<
        "limites.maxSamplerAnisotropy: " << p.limits.maxSamplerAnisotropy << std::endl <<
        "limites.maxViewports: " << p.limits.maxViewports << std::endl <<
        "limites.maxViewportDimensions: " << p.limits.maxViewportDimensions << std::endl <<
        "limites.viewportBoundsRange: " << p.limits.viewportBoundsRange << std::endl <<
        "limites.viewportSubPixelBits: " << p.limits.viewportSubPixelBits << std::endl <<
        "limites.minMemoryMapAlignment: " << p.limits.minMemoryMapAlignment << std::endl <<
        "limites.minTexelBufferOffsetAlignment: " << p.limits.minTexelBufferOffsetAlignment << std::endl <<
        "limites.minUniformBufferOffsetAlignment: " << p.limits.minUniformBufferOffsetAlignment << std::endl <<
        "limites.minStorageBufferOffsetAlignment: " << p.limits.minStorageBufferOffsetAlignment << std::endl <<
        "limites.minTexelOffset: " << p.limits.minTexelOffset << std::endl <<
        "limites.maxTexelOffset: " << p.limits.maxTexelOffset << std::endl <<
        "limites.minTexelGatherOffset: " << p.limits.minTexelGatherOffset << std::endl <<
        "limites.maxTexelGatherOffset: " << p.limits.maxTexelGatherOffset << std::endl <<
        "limites.minInterpolationOffset: " << p.limits.minInterpolationOffset << std::endl <<
        "limites.maxInterpolationOffset: " << p.limits.maxInterpolationOffset << std::endl <<
        "limites.subPixelInterpolationOffsetBits: " << p.limits.subPixelInterpolationOffsetBits << std::endl <<
        "limites.maxFramebufferWidth: " << p.limits.maxFramebufferWidth << std::endl <<
        "limites.maxFramebufferHeight: " << p.limits.maxFramebufferHeight << std::endl <<
        "limites.maxFramebufferLayers: " << p.limits.maxFramebufferLayers << std::endl <<
        "limites.framebufferColorSampleCounts: " << p.limits.framebufferColorSampleCounts << std::endl <<
        "limites.framebufferDepthSampleCounts: " << p.limits.framebufferDepthSampleCounts << std::endl <<
        "limites.framebufferStencilSampleCounts: " << p.limits.framebufferStencilSampleCounts << std::endl <<
        "limites.framebufferNoAttachmentsSampleCounts: " << p.limits.framebufferNoAttachmentsSampleCounts << std::endl <<
        "limites.maxColorAttachments: " << p.limits.maxColorAttachments << std::endl <<
        "limites.sampledImageColorSampleCounts: " << p.limits.sampledImageColorSampleCounts << std::endl <<
        "limites.sampledImageIntegerSampleCounts: " << p.limits.sampledImageIntegerSampleCounts << std::endl <<
        "limites.sampledImageDepthSampleCounts: " << p.limits.sampledImageDepthSampleCounts << std::endl <<
        "limites.sampledImageStencilSampleCounts: " << p.limits.sampledImageStencilSampleCounts << std::endl <<
        "limites.storageImageSampleCounts: " << p.limits.storageImageSampleCounts << std::endl <<
        "limites.maxSampleMaskWords: " << p.limits.maxSampleMaskWords << std::endl <<
        "limites.timestampComputeAndGraphics: " << p.limits.timestampComputeAndGraphics << std::endl <<
        "limites.timestampPeriod: " << p.limits.timestampPeriod << std::endl <<
        "limites.maxClipDistances: " << p.limits.maxClipDistances << std::endl <<
        "limites.maxCullDistances: " << p.limits.maxCullDistances << std::endl <<
        "limites.maxCombinedClipAndCullDistances: " << p.limits.maxCombinedClipAndCullDistances << std::endl <<
        "limites.discreteQueuePriorities: " << p.limits.discreteQueuePriorities << std::endl <<
        "limites.pointSizeRange: " <<  p.limits.pointSizeRange << std::endl <<
        "limites.lineWidthRange: " << p.limits.lineWidthRange << std::endl <<
        "limites.pointSizeGranularity: " << p.limits.pointSizeGranularity << std::endl <<
        "limites.lineWidthGranularity: " << p.limits.lineWidthGranularity << std::endl <<
        "limites.strictLines: " << p.limits.strictLines << std::endl <<
        "limites.standardSampleLocations: " << p.limits.standardSampleLocations << std::endl <<
        "limites.optimalBufferCopyOffsetAlignment: " << p.limits.optimalBufferCopyOffsetAlignment << std::endl <<
        "limites.optimalBufferCopyRowPitchAlignment: " << p.limits.optimalBufferCopyRowPitchAlignment << std::endl <<
        "limites.nonCoherentAtomSize: " << p.limits.nonCoherentAtomSize << std::endl <<
        "sparseProperties.residencyStandard2DBlockShape: " << p.sparseProperties.residencyStandard2DBlockShape << std::endl <<
        "sparseProperties.residencyStandard2DMultisampleBlockShape: " << p.sparseProperties.residencyStandard2DMultisampleBlockShape << std::endl <<
        "sparseProperties.residencyStandard3DBlockShape: " << p.sparseProperties.residencyStandard3DBlockShape << std::endl <<
        "sparseProperties.residencyAlignedMipSize: " << p.sparseProperties.residencyAlignedMipSize << std::endl <<
        "sparseProperties.residencyNonResidentStrict: " << p.sparseProperties.residencyNonResidentStrict << std::endl;
    return out;
}