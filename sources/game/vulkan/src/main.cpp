#include <vector>
#include <iostream>
#include <optional>
#include <tuple>
#include <algorithm>
#include <string>
#include <sstream>
std::vector<std::string> split(const std::string& s, char delimiter)
{
    std::vector<std::string> tokens;
    std::string token;
    std::istringstream tokenStream(s);
    while (std::getline(tokenStream, token, delimiter))
        tokens.push_back(token);
    return tokens;
}

#include <vulkan/vulkan.h>
#include <vulkan/vulkan_win32.h>

template <typename T>
struct unique_handle
{
    using THANDLE = typename T::HandleType;
    unique_handle(THANDLE h) : handle(h) {}

    //NON COPYABLE
    unique_handle(const unique_handle&) = delete;
    unique_handle& operator=(const unique_handle&) = delete;

    //MOVEABLE
    unique_handle(unique_handle&& other) 
        : handle(other.handle)
    {
        other.handle = T::INVALID_HANDLE;
    }   

    unique_handle& operator = (unique_handle&& other)
    {
        if (this != &other) {
            handle = other.handle;
            other.handle = T::INVALID_HANDLE;
        }
        return *this;
    }

    ~unique_handle()
    {
        if(handle != T::INVALID_HANDLE)
            T::CloseHandle(handle);
    }

    operator THANDLE() const & noexcept { return handle; }
private:
    THANDLE handle;
};

#define UNIQUEHANDLE(Name) struct Vk##Name##Methods \
{ \
    using HandleType = Vk##Name; \
    static constexpr Vk##Name INVALID_HANDLE = nullptr; \
    static void CloseHandle(Vk##Name& handle) { \
        vkDestroy##Name(handle, NULL); \
    } \
}; \
using Vk##Name##Handle = unique_handle<Vk##Name##Methods>;

#define GETINSTANCE_FUNC_PTR(instance, entrypoint) (PFN_vk##entrypoint) vkGetInstanceProcAddr(instance, "vk"#entrypoint)
#define GETDEVICE_FUNC_PTR(dev, Entrypoint) (PFN_vk##Entrypoint)vkGetDeviceProcAddr(dev, "vk"#Entrypoint)

UNIQUEHANDLE(Instance) //https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/VkInstance.html
UNIQUEHANDLE(Device) //https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/VkDevice.html

// https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/VkLayerProperties.html

VkInstanceHandle createVkInstance(char const*const appName,
    const std::string& extensionsStr = "")
{ 
    auto extensions = split(extensionsStr, ',');
    std::vector<const char *> extensionscstr;
    std::transform(extensions.begin(), extensions.end(),
        std::back_inserter(extensionscstr),
        [](auto&& x) -> auto { return x.c_str(); });

    VkInstance instance; 

    VkApplicationInfo appInfo = {}; 
    appInfo.sType                 = VK_STRUCTURE_TYPE_APPLICATION_INFO; 
    appInfo.pNext                 = NULL; 
    appInfo.pApplicationName      = appName; 
    appInfo.applicationVersion    = 1; 
    appInfo.pEngineName           = appName; 
    appInfo.engineVersion         = 1; 
    appInfo.apiVersion            = VK_API_VERSION_1_0; 

    VkInstanceCreateInfo instInfo = {}; 
    instInfo.sType                      = VK_STRUCTURE_TYPE_INSTANCE_CREATE_INFO; 
    instInfo.pNext                      = NULL; 
    instInfo.flags                      = 0; 
    instInfo.pApplicationInfo           = &appInfo; 
    //instInfo.enabledLayerCount        = layers.size(); 
    //instInfo.ppEnabledLayerNames      = layers.data(); 
    instInfo.enabledExtensionCount    = (uint32_t)extensionscstr.size(); 
    instInfo.ppEnabledExtensionNames  = extensionscstr.data(); 

    auto r = vkCreateInstance(&instInfo, NULL, &instance); 
    return {instance}; 
}

//Why is it OK to return vector from function?
//https://stackoverflow.com/a/22655160/5397116
std::vector<VkPhysicalDevice> enumeratePhysicalDevices(const VkInstanceHandle& i)
{
    uint32_t count;           
    vkEnumeratePhysicalDevices (i, &count, NULL);

    std::vector<VkPhysicalDevice> l(count);
    vkEnumeratePhysicalDevices(i, &count, l.data()); 

    return l;
}

template <typename F>
std::vector<std::tuple<uint32_t,VkQueueFamilyProperties>> enumerateQueues(VkPhysicalDevice gpu, F f)
{
    uint32_t count;
    vkGetPhysicalDeviceQueueFamilyProperties(gpu, &count, nullptr);

    std::vector<VkQueueFamilyProperties> families (count);
    vkGetPhysicalDeviceQueueFamilyProperties(gpu, &count, families.data());

    std::vector<std::tuple<uint32_t,VkQueueFamilyProperties>> r;
    uint32_t i = 0;
    for(auto&& family : families)
    { 
        if (f(i, family))
            r.emplace_back(i, family);
        ++i;
    }
    return r;
}

bool queueFamilySupports(const VkQueueFamilyProperties& family, VkQueueFlagBits flag)
{
    return (family.queueFlags & flag) == flag;
}

bool queueFamilyPresents(const VkPhysicalDevice& gpu, const VkSurfaceKHR& surface, uint32_t i)
{
    VkBool32 supports;
    vkGetPhysicalDeviceSurfaceSupportKHR(gpu, i, surface, &supports); 
    return supports;
}

using getQueueFamilyR = std::optional<std::tuple<uint32_t,VkQueueFamilyProperties>>;
template <typename F>
getQueueFamilyR getQueue(VkPhysicalDevice gpu, F f)
{
    auto queues = enumerateQueues(gpu, f);
    if (queues.size() > 0) return { queues[0] };
    else return { };
}

VkDeviceHandle createDevice(VkPhysicalDevice gpu,
    uint32_t queueFamilyIndex,
    const std::string& extensionsStr = "")
{ 
    auto extensions = split(extensionsStr, ',');
    std::vector<const char *> extensionscstr;
    std::transform(extensions.begin(), extensions.end(),
        std::back_inserter(extensionscstr),
        [](auto&& x) -> auto { return x.c_str(); });

    float queuePriorities[1]       = { 0.0 }; 

    VkDeviceQueueCreateInfo queueInfo    = {}; 
    queueInfo.queueFamilyIndex           = queueFamilyIndex;   
    queueInfo.sType                      = VK_STRUCTURE_TYPE_DEVICE_QUEUE_CREATE_INFO; 
    queueInfo.pNext                      = NULL; 
    queueInfo.queueCount                 = 1; 
    queueInfo.pQueuePriorities           = queuePriorities; 

    VkDeviceCreateInfo deviceInfo = {}; 
    deviceInfo.sType                     = VK_STRUCTURE_TYPE_DEVICE_CREATE_INFO; 
    deviceInfo.pNext                     = NULL; 
    deviceInfo.queueCreateInfoCount      = 1; 
    deviceInfo.pQueueCreateInfos         = &queueInfo; 
    deviceInfo.enabledLayerCount         = 0; 
    deviceInfo.ppEnabledLayerNames       = NULL; 
    deviceInfo.enabledExtensionCount     = (uint32_t)extensionscstr.size(); 
    deviceInfo.ppEnabledExtensionNames   = extensionscstr.data(); 
    deviceInfo.pEnabledFeatures          = NULL; 

    VkDevice device;
    auto r = vkCreateDevice(gpu, &deviceInfo, NULL, &device); 

    return { device };
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

std::vector<VkLayerProperties> vkEnumerateInstanceLayerProperties()
{
    uint32_t layersCount; 
    auto result = vkEnumerateInstanceLayerProperties(&layersCount, nullptr); 
    
    std::vector<VkLayerProperties> layers (layersCount);     
    vkEnumerateInstanceLayerProperties (&layersCount, layers.data());

    return layers;
}

std::ostream& operator << (std::ostream& out, VkLayerProperties& layer)
{
    out << layer.layerName << " " 
        << layer.specVersion << " "
        << layer.implementationVersion << " "
        << layer.description << std::endl;
    return out;
}

VkSurfaceKHR createVkSurface(HINSTANCE hInstance, HWND hwnd, const VkInstance& instance)
{
    //auto VkCreateWin32SurfaceKHR = (PFN_vkCreateWin32SurfaceKHR)vkGetInstanceProcAddr(instance, "vkCreateWin32SurfaceKHR");

    VkSurfaceKHR surface;
    VkWin32SurfaceCreateInfoKHR createInfo = {}; 
    createInfo.sType         = VK_STRUCTURE_TYPE_WIN32_SURFACE_CREATE_INFO_KHR; 
    createInfo.pNext         = NULL;
    createInfo.hinstance     = hInstance; 
    createInfo.hwnd          = hwnd; 
    auto r = vkCreateWin32SurfaceKHR(instance, &createInfo, nullptr, &surface);

    return surface;
}

VkQueue getQueue(VkDevice device, uint32_t family, uint32_t queue)
{
    VkQueue q;
    vkGetDeviceQueue(device, family, queue, &q);

    return q;
}

VkSwapchainKHR createSwapChain(VkPhysicalDevice gpu, VkDevice device, VkSurfaceKHR surface)
{
    uint32_t count;
    vkGetPhysicalDeviceSurfaceFormatsKHR(gpu, surface, &count, nullptr);

    std::vector<VkSurfaceFormatKHR> formats(count);
    vkGetPhysicalDeviceSurfaceFormatsKHR(gpu, surface, &count, formats.data());

    VkSurfaceCapabilitiesKHR surfaceCapabilities;
    vkGetPhysicalDeviceSurfaceCapabilitiesKHR(gpu, surface, &surfaceCapabilities);

    // https://khronos.org/registry/vulkan/specs/1.1-extensions/man/html/VkSwapchainCreateInfoKHR.html
    VkSwapchainCreateInfoKHR scInfo = {}; 
    scInfo.sType               = VK_STRUCTURE_TYPE_SWAPCHAIN_CREATE_INFO_KHR; 
    scInfo.pNext               = NULL; 
    scInfo.surface             = surface; 
    scInfo.minImageCount       = 2; 
    scInfo.imageFormat         = formats[0].format;
    scInfo.imageExtent.width   = surfaceCapabilities.currentExtent.width; 
    scInfo.imageExtent.height  = surfaceCapabilities.currentExtent.height; 
    scInfo.preTransform        = surfaceCapabilities.currentTransform; 
    scInfo.compositeAlpha      = VK_COMPOSITE_ALPHA_OPAQUE_BIT_KHR; 
    scInfo.imageArrayLayers    = 1; 
    scInfo.presentMode         = VK_PRESENT_MODE_FIFO_KHR; 
    scInfo.oldSwapchain        = VK_NULL_HANDLE; 
    scInfo.clipped             = true; 
    scInfo.imageColorSpace     = VK_COLOR_SPACE_SRGB_NONLINEAR_KHR; 
    scInfo.imageUsage          =      
                VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT |  
                VK_IMAGE_USAGE_TRANSFER_DST_BIT; 
    scInfo.imageSharingMode         = VK_SHARING_MODE_EXCLUSIVE; 
    scInfo.queueFamilyIndexCount    = 0; 
    scInfo.pQueueFamilyIndices      = NULL;
    VkSwapchainKHR swapChain;
    auto r = vkCreateSwapchainKHR(device, &scInfo, nullptr, &swapChain); 

    return swapChain;
}

VkQueue& operator << (VkQueue& queue, VkPresentInfoKHR& info)
{
    vkQueuePresentKHR(queue, &info);
    return queue;
}

VkPresentInfoKHR present(VkSwapchainKHR& swapChain, uint32_t imgIndex)
{
    return VkPresentInfoKHR{VK_STRUCTURE_TYPE_PRESENT_INFO_KHR, nullptr, 0, nullptr, 1, &swapChain, &imgIndex, nullptr };
}

uint32_t waitAcquireNextImage(const VkDevice& device, const VkSwapchainKHR& swapChain)
{
    uint32_t imgIndex;
    vkAcquireNextImageKHR(device, swapChain, 
        UINT64_MAX, 
        VK_NULL_HANDLE, VK_NULL_HANDLE, 
        &imgIndex);
    return imgIndex;
}

#include <Windows.h>

LRESULT CALLBACK WndProc(HWND hwnd, UINT msg, WPARAM wParam, LPARAM lParam)
{
	switch (msg)
	{
		case WM_CLOSE: DestroyWindow(hwnd); break;
		case WM_DESTROY: PostQuitMessage(0); break;
		default: return DefWindowProcW(hwnd, msg, wParam, lParam);
	}
	return 0;
}

HWND createWindow(HINSTANCE hInstance, const std::wstring& className, int nCmdShow)
{
  WNDCLASSEXW wc;
	wc.cbSize = sizeof(WNDCLASSEX);
	wc.style = 0;
	wc.lpfnWndProc = WndProc;
	wc.cbClsExtra = 0;
	wc.cbWndExtra = 0;
	wc.hInstance = hInstance;
	wc.hIcon = LoadIcon(NULL, IDI_APPLICATION);
	wc.hCursor = LoadCursor(NULL, IDC_ARROW);
	wc.hbrBackground = (HBRUSH)(COLOR_WINDOW + 1);
	wc.lpszMenuName = NULL;
	wc.lpszClassName = className.c_str();
	wc.hIconSm = LoadIcon(NULL, IDI_APPLICATION);
	RegisterClassExW(&wc);

    
	HWND hwnd = CreateWindowExW(
		WS_EX_CLIENTEDGE,
		className.c_str(),
		L"",
		WS_OVERLAPPEDWINDOW,
		CW_USEDEFAULT, CW_USEDEFAULT, 
		640, 480,
		NULL, NULL, hInstance, NULL);
	ShowWindow(hwnd, nCmdShow);
	UpdateWindow(hwnd);

    return hwnd;
}


struct LoopThreadParams
{
    HINSTANCE hInstance;
	HWND hwnd;
};

DWORD WINAPI LoopThread(LPVOID lpParam)
{
    auto& p = *(LoopThreadParams*)lpParam;
    auto& hInstance = p.hInstance;
    auto& hwnd = p.hwnd;

    auto instance = createVkInstance("Vulkan Test", 
        VK_KHR_SURFACE_EXTENSION_NAME "," VK_KHR_WIN32_SURFACE_EXTENSION_NAME);
    auto surface = createVkSurface(hInstance, hwnd, instance);

    auto devices = enumeratePhysicalDevices(instance);
    auto gpu0 = devices[0];
    auto [index, queueFamily] = getQueue(gpu0, [&](auto i, auto&& f)
        { 
            return queueFamilySupports(f, VK_QUEUE_GRAPHICS_BIT) && queueFamilyPresents(gpu0, surface, i); 
        }).value();    
    auto device = createDevice(gpu0, index, VK_KHR_SWAPCHAIN_EXTENSION_NAME);

    auto queue = getQueue(device, index, 0);
    auto swapChain = createSwapChain(gpu0, device, surface);

    while(true) 
    {
        auto imgIndex = waitAcquireNextImage(device, swapChain);
        queue << present(swapChain, imgIndex);        
    }
}

int WINAPI WinMain(HINSTANCE hInstance,
    HINSTANCE hPrevInstance,
	LPSTR lpCmdLine, int nCmdShow)
{
    auto hwnd = createWindow(hInstance, L"vulcanWindow", nCmdShow);

	LoopThreadParams p = { hInstance, hwnd };
	DWORD   dwThreadIdArray;
	HANDLE  hThreadArray = CreateThread(
		NULL,
		0,
		LoopThread,
		&p,
		0,
		&dwThreadIdArray);

    MSG msg;
	while(GetMessage(&msg, NULL, 0, 0 ))
    { 
		TranslateMessage(&msg); 
		DispatchMessage(&msg); 
    } 
    return 0;
}