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
#include "uniqueHandle.h"

#define GETINSTANCE_FUNC_PTR(instance, entrypoint) (PFN_vk##entrypoint) vkGetInstanceProcAddr(instance, "vk"#entrypoint)
#define GETDEVICE_FUNC_PTR(dev, Entrypoint) (PFN_vk##Entrypoint)vkGetDeviceProcAddr(dev, "vk"#Entrypoint)

#define VULKANHANDLE(Name) struct Vk##Name##Methods \
{ \
    using HandleType = Vk##Name; \
    static constexpr Vk##Name INVALID_HANDLE = nullptr; \
    static void CloseHandle(Vk##Name& handle) { \
        vkDestroy##Name(handle, NULL); \
    } \
}; \
using Vk##Name##Handle = unique_handle<Vk##Name##Methods>;
VULKANHANDLE(Instance) //https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/VkInstance.html
VULKANHANDLE(Device) //https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/VkDevice.html

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

std::vector<VkLayerProperties> vkEnumerateInstanceLayerProperties()
{
    uint32_t layersCount; 
    auto result = vkEnumerateInstanceLayerProperties(&layersCount, nullptr); 
    
    std::vector<VkLayerProperties> layers (layersCount);     
    vkEnumerateInstanceLayerProperties (&layersCount, layers.data());

    return layers;
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