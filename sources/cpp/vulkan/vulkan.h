#pragma once

#include <experimental/generator>
#include <string>
#include <sstream>
#include <vector>
#include <algorithm>
#include <memory>
#include <optional>
#include <fstream>

std::vector<std::string> split(const std::string& s, char delimiter)
{
    std::vector<std::string> v;

    std::string token;
    std::istringstream tokenStream(s);
    while (std::getline(tokenStream, token, delimiter))
        v.push_back(token);

    return v;
}

std::vector<const char*> as_argv(const std::vector<std::string>& items)
{
    std::vector<const char*> argv;
    std::transform(items.begin(), items.end(),
        std::back_inserter(argv),
        [](auto&& x) -> auto { return x.c_str(); });
    return argv;
}

#include <vulkan/vulkan.h>
#include <vulkan/vulkan_win32.h>

#define VULKANHANDLE(Name) struct DestroyVk##Name { void operator () (Vk##Name##_T* ptr) { if(ptr != nullptr) vkDestroy##Name##(ptr, nullptr); } }; \
using Vk##Name##Handle = std::unique_ptr<Vk##Name##_T, DestroyVk##Name>;

#define VULKANPARAM(Name, T) struct DestroyVk##Name { T param; DestroyVk##Name(T p1) : param{p1} {} void operator () (Vk##Name##_T* ptr) { if(ptr != nullptr) vkDestroy##Name##(param, ptr, nullptr); } }; \
using Vk##Name##Handle = std::unique_ptr<Vk##Name##_T, DestroyVk##Name>;

VULKANHANDLE(Instance) //https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/VkInstance.html
// https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/VkPhysicalDevice.html
VULKANHANDLE(Device) //https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/VkDevice.html
// https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/VkQueue.html

VULKANPARAM(SurfaceKHR, VkInstance) //https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/VkSurfaceKHR.html
VULKANPARAM(SwapchainKHR, VkDevice) //https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/VkSwapchainKHR.html

VULKANPARAM(ShaderModule, VkDevice) //https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/VkShaderModule.html
VULKANPARAM(PipelineLayout, VkDevice) //https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/VkShaderModule.html
VULKANPARAM(Pipeline, VkDevice) //https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/VkPipeline.html
VULKANPARAM(RenderPass, VkDevice) //https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/VkRenderPass.html

uint32_t findMemoryType(const VkPhysicalDevice& gpu, uint32_t typeFilter, VkMemoryPropertyFlags properties) {
    VkPhysicalDeviceMemoryProperties memProperties;
    vkGetPhysicalDeviceMemoryProperties(gpu, &memProperties);
    for (uint32_t i = 0; i < memProperties.memoryTypeCount; i++) {
        if ((typeFilter & (1 << i)) && (memProperties.memoryTypes[i].propertyFlags & properties) == properties) {
            return i;
        }
    }
}

VkDeviceMemory valloc(const VkPhysicalDevice& gpu, 
    const VkDevice& device, 
    VkMemoryAllocateInfo alloc_info)
{
    alloc_info.sType = VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_INFO;
    alloc_info.pNext = nullptr;
    alloc_info.memoryTypeIndex = findMemoryType(
        gpu, 
        (uint32_t)alloc_info.allocationSize,
        VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT | VK_MEMORY_PROPERTY_HOST_COHERENT_BIT);

    VkDeviceMemory mem;
    auto r = vkAllocateMemory(device, &alloc_info, nullptr, &mem);

    return mem;
}

std::vector<VkLayerProperties> get_extensions()
{
    uint32_t layerCount;
    vkEnumerateInstanceLayerProperties(&layerCount, nullptr);

    std::vector<VkLayerProperties> availableLayers(layerCount);
    vkEnumerateInstanceLayerProperties(&layerCount, availableLayers.data());

    return availableLayers;
}

std::optional<VkInstanceHandle> createVkInstance(char const* const appName,
    const std::string& extensionsStr = "",
    const std::string& layersStr = "")
{
    auto extensions = split(extensionsStr, ',');
    auto exts = as_argv(extensions);

    auto layers = split(layersStr, ',');
    auto ls = as_argv(layers);

    VkInstance instance;

    VkApplicationInfo appInfo = {};
    appInfo.sType = VK_STRUCTURE_TYPE_APPLICATION_INFO;
    appInfo.pNext = NULL;
    appInfo.pApplicationName = appName;
    appInfo.applicationVersion = 1;
    appInfo.pEngineName = appName;
    appInfo.engineVersion = 1;
    appInfo.apiVersion = VK_API_VERSION_1_0;

    VkInstanceCreateInfo instInfo = {};
    instInfo.sType = VK_STRUCTURE_TYPE_INSTANCE_CREATE_INFO;
    instInfo.pNext = NULL;
    instInfo.flags = 0;
    instInfo.pApplicationInfo = &appInfo;
    instInfo.enabledLayerCount = (uint32_t)ls.size();
    instInfo.ppEnabledLayerNames = ls.data(); 
    instInfo.enabledExtensionCount = (uint32_t)exts.size();
    instInfo.ppEnabledExtensionNames = exts.data();

    auto r = vkCreateInstance(&instInfo, NULL, &instance);
    if (r == VkResult::VK_SUCCESS)
        return VkInstanceHandle{ std::move(instance), {} };
    else
        return {};
}

std::vector<VkPhysicalDevice> enumeratePhysicalDevices(const VkInstance& instance)
{
    uint32_t count;
    vkEnumeratePhysicalDevices(instance, &count, NULL);

    std::vector<VkPhysicalDevice> l(count);
    vkEnumeratePhysicalDevices(instance, &count, l.data());

    return l;
}

template <typename F>
std::vector<std::tuple<uint32_t, VkQueueFamilyProperties>> enumerateQueues(const VkPhysicalDevice& gpu, F predicate)
{
    uint32_t count;
    vkGetPhysicalDeviceQueueFamilyProperties(gpu, &count, nullptr);

    std::vector<VkQueueFamilyProperties> families(count);
    vkGetPhysicalDeviceQueueFamilyProperties(gpu, &count, families.data());

    std::vector<std::tuple<uint32_t, VkQueueFamilyProperties>> r;
    uint32_t i = 0;
    for (auto&& family : families)
    {
        if (predicate(i, family))
            r.emplace_back(i, family);
        ++i;
    }
    return r;
}

bool queueFamilySupports(const VkQueueFamilyProperties& family, VkQueueFlagBits flag)
{
    return (family.queueFlags & flag) == flag;
}

bool queueFamilyPresentsAt(const VkPhysicalDevice& gpu, uint32_t i, const VkSurfaceKHR& surface)
{
    VkBool32 supports;
    vkGetPhysicalDeviceSurfaceSupportKHR(gpu, i, surface, &supports);
    return supports;
}

template <typename F>
std::optional<std::tuple<uint32_t, VkQueueFamilyProperties>> getQueue(VkPhysicalDevice gpu, F predicate)
{
    auto queues = enumerateQueues(gpu, predicate);
    if (queues.size() > 0) return { queues[0] };
    else return { };
}

std::optional<VkSurfaceKHRHandle> createVkSurface(HINSTANCE hInstance, HWND hwnd, const VkInstance& instance)
{
    //auto VkCreateWin32SurfaceKHR = (PFN_vkCreateWin32SurfaceKHR)vkGetInstanceProcAddr(instance, "vkCreateWin32SurfaceKHR");

    VkSurfaceKHR surface;
    VkWin32SurfaceCreateInfoKHR createInfo = {};
    createInfo.sType = VK_STRUCTURE_TYPE_WIN32_SURFACE_CREATE_INFO_KHR;
    createInfo.pNext = NULL;
    createInfo.hinstance = hInstance;
    createInfo.hwnd = hwnd;
    auto r = vkCreateWin32SurfaceKHR(instance, &createInfo, nullptr, &surface);
    if (r == VK_SUCCESS)
        return VkSurfaceKHRHandle{ surface, {instance} };
    else 
        return {};
}

std::optional<VkDeviceHandle> createDevice(VkPhysicalDevice gpu,
    uint32_t queueFamilyIndex,
    const std::string& extensionsStr = "")
{
    auto extensions = split(extensionsStr, ',');
    auto exts = as_argv(extensions);

    float queuePriorities[1] = { 0.0 };

    VkDeviceQueueCreateInfo queueInfo = {};
    queueInfo.queueFamilyIndex = queueFamilyIndex;
    queueInfo.sType = VK_STRUCTURE_TYPE_DEVICE_QUEUE_CREATE_INFO;
    queueInfo.pNext = NULL;
    queueInfo.queueCount = 1;
    queueInfo.pQueuePriorities = queuePriorities;

    VkDeviceCreateInfo deviceInfo = {};
    deviceInfo.sType = VK_STRUCTURE_TYPE_DEVICE_CREATE_INFO;
    deviceInfo.pNext = NULL;
    deviceInfo.queueCreateInfoCount = 1;
    deviceInfo.pQueueCreateInfos = &queueInfo;
    deviceInfo.enabledLayerCount = 0;
    deviceInfo.ppEnabledLayerNames = NULL;
    deviceInfo.enabledExtensionCount = (uint32_t)exts.size();
    deviceInfo.ppEnabledExtensionNames = exts.data();
    deviceInfo.pEnabledFeatures = NULL;

    VkDevice device;
    auto r = vkCreateDevice(gpu, &deviceInfo, NULL, &device);
    if (r == VK_SUCCESS)
        return VkDeviceHandle{ device, {} };
    else
        return {};
}

std::optional<VkQueue> getQueue(const VkDevice& device, uint32_t family, uint32_t queue)
{
    VkQueue q;
    vkGetDeviceQueue(device, family, queue, &q);
    if (q != nullptr)
        return { q };
    else 
        return {};
}

std::optional<std::tuple<VkSwapchainKHRHandle, VkSurfaceFormatKHR, VkSurfaceCapabilitiesKHR>> createSwapChain(VkPhysicalDevice gpu, const VkDevice& device, const VkSurfaceKHR& surface)
{
    uint32_t count;
    vkGetPhysicalDeviceSurfaceFormatsKHR(gpu, surface, &count, nullptr);

    std::vector<VkSurfaceFormatKHR> formats(count);
    vkGetPhysicalDeviceSurfaceFormatsKHR(gpu, surface, &count, formats.data());

    VkSurfaceCapabilitiesKHR surfaceCapabilities;
    vkGetPhysicalDeviceSurfaceCapabilitiesKHR(gpu, surface, &surfaceCapabilities);

    // https://khronos.org/registry/vulkan/specs/1.1-extensions/man/html/VkSwapchainCreateInfoKHR.html
    VkSwapchainCreateInfoKHR scInfo = {};
    scInfo.sType = VK_STRUCTURE_TYPE_SWAPCHAIN_CREATE_INFO_KHR;
    scInfo.pNext = NULL;
    scInfo.surface = surface;
    scInfo.minImageCount = 2;
    scInfo.imageFormat = formats[0].format;
    scInfo.imageExtent.width = surfaceCapabilities.currentExtent.width;
    scInfo.imageExtent.height = surfaceCapabilities.currentExtent.height;
    scInfo.preTransform = surfaceCapabilities.currentTransform;
    scInfo.compositeAlpha = VK_COMPOSITE_ALPHA_OPAQUE_BIT_KHR;
    scInfo.imageArrayLayers = 1;
    scInfo.presentMode = VK_PRESENT_MODE_FIFO_KHR;
    scInfo.oldSwapchain = VK_NULL_HANDLE;
    scInfo.clipped = true;
    scInfo.imageColorSpace = VK_COLOR_SPACE_SRGB_NONLINEAR_KHR;
    scInfo.imageUsage =
        VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT |
        VK_IMAGE_USAGE_TRANSFER_DST_BIT;
    scInfo.imageSharingMode = VK_SHARING_MODE_EXCLUSIVE;
    scInfo.queueFamilyIndexCount = 0;
    scInfo.pQueueFamilyIndices = NULL;

    VkSwapchainKHR swapChain;
    auto r = vkCreateSwapchainKHR(device, &scInfo, nullptr, &swapChain);
    if (r == VK_SUCCESS)
        return std::make_tuple( VkSwapchainKHRHandle{ swapChain, {device} }, formats[0], surfaceCapabilities );
    else
        return {};
}

uint32_t waitAcquireNextImage(const VkDevice& device, const VkSwapchainKHR& swapChain, const VkSemaphore& semaphore)
{
    
    uint32_t imgIndex;
    vkAcquireNextImageKHR(device, swapChain,
        UINT64_MAX,
        semaphore, VK_NULL_HANDLE,
        &imgIndex);
    return imgIndex;
}

// Pipeline

std::optional<VkShaderModule> createShaderModule(const VkDevice& device, const std::vector<uint8_t>& spirv)
{
    VkShaderModuleCreateInfo createInfo = {
        VkStructureType::VK_STRUCTURE_TYPE_SHADER_MODULE_CREATE_INFO,
        nullptr,
        0,
        spirv.size(),
        (const uint32_t*)spirv.data()
    };

    VkShaderModule shaderModule;
    auto r = vkCreateShaderModule(device, &createInfo, nullptr, &shaderModule);
    if (r == VkResult::VK_SUCCESS)
        return shaderModule;
    else
        return {};
}

std::optional<VkShaderModule> createShaderModule(const VkDevice& device, const std::string& filePath, size_t max_size = std::numeric_limits<size_t>::max())
{
    std::ifstream file{ filePath, std::ios::ate | std::ios::binary };
    if (!file.is_open()) return {};

    auto pos = file.tellg();
    if (pos == -1) return {};

    auto file_size = (size_t)pos;
    if (file_size > max_size) return {};

    std::vector<uint8_t> buffer(file_size);
    file.seekg(0).read((char*)buffer.data(), file_size);
    file.close();

    return createShaderModule(device, buffer);
}

VkPipelineShaderStageCreateInfo createPipelineStage(const VkShaderModule& module, VkShaderStageFlagBits type, const char* mainName)
{
    VkPipelineShaderStageCreateInfo vertShaderStageInfo = {
        VK_STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_CREATE_INFO,
        nullptr,
        0,
        type,
        module,
        mainName,
        nullptr
    };
    return vertShaderStageInfo;
}

VkPipelineVertexInputStateCreateInfo createVertexInput()
{
    VkPipelineVertexInputStateCreateInfo vertexInputInfo = {
        VK_STRUCTURE_TYPE_PIPELINE_VERTEX_INPUT_STATE_CREATE_INFO,
        nullptr,
        0,
        0,
        nullptr,
        0,
        nullptr
    };
    return vertexInputInfo;
}

VkPipelineInputAssemblyStateCreateInfo createVertexAssembly()
{
    VkPipelineInputAssemblyStateCreateInfo inputAssembly = {
        VK_STRUCTURE_TYPE_PIPELINE_INPUT_ASSEMBLY_STATE_CREATE_INFO,
        nullptr,
        0,
        VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST,
        VK_FALSE
    };
    return inputAssembly;
}

VkViewport createViewport(float width, float height)
{
    VkViewport viewport = {
        0.0f, 0.0f,
        width, height,
        0.0f, 1.0f
    };
    return viewport;
}

VkViewport createViewport(const VkPhysicalDevice& gpu, const VkSurfaceKHR& surface)
{
    VkSurfaceCapabilitiesKHR surfaceCapabilities;
    vkGetPhysicalDeviceSurfaceCapabilitiesKHR(gpu, surface, &surfaceCapabilities);
    return createViewport(
        (float)surfaceCapabilities.currentExtent.width, 
        (float)surfaceCapabilities.currentExtent.height
    );
}

VkRect2D createScissor(VkExtent2D extent)
{
    VkRect2D scissor = {
        { 0, 0 },
        extent
    };
    return scissor;
}

VkRect2D createScissor(const VkPhysicalDevice& gpu, const VkSurfaceKHR& surface)
{
    VkSurfaceCapabilitiesKHR surfaceCapabilities;
    vkGetPhysicalDeviceSurfaceCapabilitiesKHR(gpu, surface, &surfaceCapabilities);
    VkRect2D scissor = {
        { 0, 0 },
        surfaceCapabilities.currentExtent
    };
    return scissor;
}

template <typename T, typename... TArgs>
struct creationInfoHelper
{
    std::shared_ptr<std::tuple<TArgs...>> args;
    T info;

    creationInfoHelper(TArgs... a)
    {
        args = std::make_shared<std::tuple<TArgs...>>(a...);
    }
    operator const T& () { return info; }
};

VkPipelineViewportStateCreateInfo createViewportState(VkViewport& viewport, VkRect2D& scissors)
{
    VkPipelineViewportStateCreateInfo viewportState = {
        VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_STATE_CREATE_INFO,
        nullptr,
        0,
        1,
        &viewport,
        1,
        &scissors
    };
    return viewportState;
}

using viewportState = creationInfoHelper<VkPipelineViewportStateCreateInfo, VkViewport, VkRect2D>;
using colorBlendState = creationInfoHelper<VkPipelineColorBlendStateCreateInfo, VkPipelineColorBlendAttachmentState>;

viewportState createViewportState(const VkPhysicalDevice& gpu, const VkSurfaceKHR& surface)
{
    auto r = viewportState{ createViewport(gpu, surface), createScissor(gpu, surface) };
    r.info = createViewportState(
        std::get<0>(*r.args),
        std::get<1>(*r.args));
    return r;
}

VkPipelineRasterizationStateCreateInfo createRasterizer()
{
    VkPipelineRasterizationStateCreateInfo rasterizer = {
        VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_STATE_CREATE_INFO,
        nullptr,
        0,
        VK_FALSE,                   //depthClampEnable;
        VK_FALSE,                   //rasterizerDiscardEnable 
        VK_POLYGON_MODE_FILL,       //polygonMode
        VK_CULL_MODE_BACK_BIT,      //cullMode 
        VK_FRONT_FACE_CLOCKWISE,    //frontFace 
        VK_FALSE,                   //depthBiasEnable 
        0.0f,                       //depthBiasConstantFactor;
        0.0f,                       //depthBiasClamp;
        0.0f,                       //depthBiasSlopeFactor;
        1.0f                        //lineWidth
    };
    return rasterizer;
}

VkPipelineMultisampleStateCreateInfo createMultisampling()
{
    VkPipelineMultisampleStateCreateInfo multisampling = {
        VK_STRUCTURE_TYPE_PIPELINE_MULTISAMPLE_STATE_CREATE_INFO,
        nullptr,
        0,
        VK_SAMPLE_COUNT_1_BIT,  //rasterizationSamples;
        VK_FALSE,               //sampleShadingEnable;
        1.0f,                   //minSampleShading;
        nullptr,                //pSampleMask;
        VK_FALSE,               //alphaToCoverageEnable;
        VK_FALSE,               //alphaToOneEnable;
    };
    return multisampling;
}

colorBlendState createColorBlend()
{
    VkPipelineColorBlendAttachmentState colorBlendAttachment = {
        VK_FALSE,                        //blendEnable;
        VK_BLEND_FACTOR_ONE,             //srcColorBlendFactor;
        VK_BLEND_FACTOR_ZERO,            //dstColorBlendFactor;
        VK_BLEND_OP_ADD,                 //colorBlendOp;
        VK_BLEND_FACTOR_ONE,             //srcAlphaBlendFactor;
        VK_BLEND_FACTOR_ZERO,            // dstAlphaBlendFactor;
        VK_BLEND_OP_ADD,                 //alphaBlendOp;
        VK_COLOR_COMPONENT_R_BIT |
            VK_COLOR_COMPONENT_G_BIT |
            VK_COLOR_COMPONENT_B_BIT |
            VK_COLOR_COMPONENT_A_BIT     //colorWriteMask;
    };

    auto r = colorBlendState{ colorBlendAttachment };
    r.info = {
        VK_STRUCTURE_TYPE_PIPELINE_COLOR_BLEND_STATE_CREATE_INFO,
        nullptr,
        0,
        VK_FALSE,                   //logicOpEnable;
        VK_LOGIC_OP_COPY,           //logicOp;
        1,                          //attachmentCount;
        &std::get<0>(*r.args),       //pAttachments 
        {0.0f, 0.0f, 0.0f, 0.0f }
    };

    return r;
}

VkPipelineLayoutCreateInfo make_pipeline_info()
{
    VkPipelineLayoutCreateInfo pipelineLayoutInfo = {
        VK_STRUCTURE_TYPE_PIPELINE_LAYOUT_CREATE_INFO,
        nullptr,
        0,
        0,       //setLayoutCount;
        nullptr, //pSetLayouts;
        0,       //pushConstantRangeCount;
        nullptr, //pPushConstantRanges;
    };
    return pipelineLayoutInfo;
}

std::optional<VkPipelineLayoutHandle> createPipelineLayout(const VkDevice& device)
{
    VkPipelineLayoutCreateInfo pipelineLayoutInfo = {
        VK_STRUCTURE_TYPE_PIPELINE_LAYOUT_CREATE_INFO,
        nullptr,
        0,
        0,       //setLayoutCount;
        nullptr, //pSetLayouts;
        0,       //pushConstantRangeCount;
        nullptr, //pPushConstantRanges;
    };

    VkPipelineLayout pipelineLayout;
    auto r = vkCreatePipelineLayout(device, &pipelineLayoutInfo, nullptr, &pipelineLayout);
    if (r == VkResult::VK_SUCCESS)
        return VkPipelineLayoutHandle{ pipelineLayout, {device} };
    else
        return {};
}

std::optional<VkRenderPassHandle> createRenderPass(const VkPhysicalDevice& gpu,
    const VkSurfaceKHR& surface,
    const VkDevice& device) {

    uint32_t count;
    vkGetPhysicalDeviceSurfaceFormatsKHR(gpu, surface, &count, nullptr);

    std::vector<VkSurfaceFormatKHR> formats(count);
    vkGetPhysicalDeviceSurfaceFormatsKHR(gpu, surface, &count, formats.data());

    VkAttachmentDescription colorAttachment = {};
    colorAttachment.format = formats[0].format;
    colorAttachment.samples = VK_SAMPLE_COUNT_1_BIT;
    colorAttachment.loadOp = VK_ATTACHMENT_LOAD_OP_CLEAR;
    colorAttachment.storeOp = VK_ATTACHMENT_STORE_OP_STORE;
    colorAttachment.stencilLoadOp = VK_ATTACHMENT_LOAD_OP_DONT_CARE;
    colorAttachment.stencilStoreOp = VK_ATTACHMENT_STORE_OP_DONT_CARE;
    colorAttachment.initialLayout = VK_IMAGE_LAYOUT_UNDEFINED;
    colorAttachment.finalLayout = VK_IMAGE_LAYOUT_PRESENT_SRC_KHR;

    VkAttachmentReference colorAttachmentRef = {};
    colorAttachmentRef.attachment = 0;
    colorAttachmentRef.layout = VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL;

    VkSubpassDescription subpass = {};
    subpass.pipelineBindPoint = VK_PIPELINE_BIND_POINT_GRAPHICS;
    subpass.colorAttachmentCount = 1;
    subpass.pColorAttachments = &colorAttachmentRef;

    VkRenderPassCreateInfo renderPassInfo = {};
    renderPassInfo.sType = VK_STRUCTURE_TYPE_RENDER_PASS_CREATE_INFO;
    renderPassInfo.attachmentCount = 1;
    renderPassInfo.pAttachments = &colorAttachment;
    renderPassInfo.subpassCount = 1;
    renderPassInfo.pSubpasses = &subpass;

    VkRenderPass renderPass; 
    auto r = vkCreateRenderPass(device, &renderPassInfo, nullptr, &renderPass);
    if (r == VkResult::VK_SUCCESS)
        return VkRenderPassHandle{ renderPass, {device} };
    else
        return {};
}

std::optional<VkPipelineHandle> createPipeline(
    const VkDevice& device,
    const VkPipelineShaderStageCreateInfo& vertex,
    const VkPipelineShaderStageCreateInfo& frag,
    const VkPipelineVertexInputStateCreateInfo& vertex_input,
    const VkPipelineInputAssemblyStateCreateInfo& input_assembler,
    const VkPipelineViewportStateCreateInfo& viewport,
    const VkPipelineRasterizationStateCreateInfo& rasterizer,
    const VkPipelineMultisampleStateCreateInfo& multisample,
    const VkPipelineColorBlendStateCreateInfo& colorBlend,
    const VkPipelineLayout& layout,
    const VkRenderPass& renderPass)
{
    VkPipelineShaderStageCreateInfo stages[2] = { vertex, frag };

    VkGraphicsPipelineCreateInfo pipelineInfo = {
        VK_STRUCTURE_TYPE_GRAPHICS_PIPELINE_CREATE_INFO,
        nullptr,
        0,
        2,                  //stageCount;
        stages,             //pStages;
        &vertex_input,      //pVertexInputState;
        &input_assembler,   //pInputAssemblyState;
        nullptr,            //pTessellationState;
        &viewport,          //pViewportState;
        &rasterizer,        //pRasterizationState;
        &multisample,       //pMultisampleState;
        nullptr,            //pDepthStencilState;
        &colorBlend,        //pColorBlendState;
        nullptr,            //pDynamicState;
        layout,             //layout;
        renderPass,         //renderPass;
        0,                  //subpass;
        VK_NULL_HANDLE,     //basePipelineHandle;
        0                   //basePipelineIndex;
    };

    VkPipeline graphicsPipeline;
    auto r = vkCreateGraphicsPipelines(device, VK_NULL_HANDLE, 1, &pipelineInfo, nullptr, &graphicsPipeline);
    if (r == VkResult::VK_SUCCESS)
        return VkPipelineHandle{ graphicsPipeline, {device} };
    else
        return {};
}

// Queue Commands

VkQueue& operator << (VkQueue& queue, const VkPresentInfoKHR& info)
{
    vkQueuePresentKHR(queue, &info);
    return queue;
}

VkPresentInfoKHR present(const VkSwapchainKHR& swapChain, uint32_t imgIndex, const VkSemaphore& semaphore)
{
    return VkPresentInfoKHR{
        VkStructureType::VK_STRUCTURE_TYPE_PRESENT_INFO_KHR, 
        nullptr, 
        1, 
        &semaphore,
        1, 
        &swapChain, 
        &imgIndex, 
        nullptr 
    };
}

