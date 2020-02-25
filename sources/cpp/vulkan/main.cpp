
#define NOMINMAX
#include <Windows.h>
#include "vulkan.h"
#include "./models/Boy01_Head_GeoMesh.vulkan.h"
#include "./shaders/vert.h"

struct LoopThreadParams
{
	HINSTANCE hInstance;
	HWND hWnd;
};

DWORD WINAPI LoopThread(LPVOID lpParam)
{
	auto& params = *(LoopThreadParams*)lpParam;
	auto& hWnd = params.hWnd;
	auto& hInstance = params.hInstance;

	auto extensions = get_extensions();

	auto vk = std::move(
		createVkInstance("vulkan",
			VK_KHR_SURFACE_EXTENSION_NAME "," VK_KHR_WIN32_SURFACE_EXTENSION_NAME, "VK_LAYER_KHRONOS_validation")
		.value());

	auto surface = std::move(
		createVkSurface(hInstance, hWnd, &*vk)
		.value());

	auto gpus = enumeratePhysicalDevices(&*vk);
	auto gpu0 = gpus[0];
	auto [family_index, queueFamily] = getQueue(gpu0, [&](auto i, auto&& f)
	{
		return queueFamilySupports(f, VK_QUEUE_GRAPHICS_BIT) && queueFamilyPresentsAt(gpu0, i, &*surface);
	}).value();

	auto device = std::move(
		createDevice(gpu0, family_index, VK_KHR_SWAPCHAIN_EXTENSION_NAME)
		.value());
	auto queue = getQueue(&*device, family_index, 0).value();
	auto [swapChain, format, extent] = std::move(createSwapChain(gpu0, &*device, &*surface).value());

	std::vector<VkImage> swapChainImages;
	uint32_t imageCount;
	vkGetSwapchainImagesKHR(&*device, &*swapChain, &imageCount, nullptr);
	swapChainImages.resize(imageCount);
	vkGetSwapchainImagesKHR(&*device, &*swapChain, &imageCount, swapChainImages.data());

	std::vector<VkImageView> swapChainImageViews(swapChainImages.size());
	for (size_t i = 0; i < swapChainImages.size(); i++) {
		VkImageViewCreateInfo createInfo = {};
		createInfo.sType = VK_STRUCTURE_TYPE_IMAGE_VIEW_CREATE_INFO;
		createInfo.image = swapChainImages[i];
		createInfo.viewType = VK_IMAGE_VIEW_TYPE_2D;
		createInfo.format = format.format;
		createInfo.components.r = VK_COMPONENT_SWIZZLE_IDENTITY;
		createInfo.components.g = VK_COMPONENT_SWIZZLE_IDENTITY;
		createInfo.components.b = VK_COMPONENT_SWIZZLE_IDENTITY;
		createInfo.components.a = VK_COMPONENT_SWIZZLE_IDENTITY;
		createInfo.subresourceRange.aspectMask = VK_IMAGE_ASPECT_COLOR_BIT;
		createInfo.subresourceRange.baseMipLevel = 0;
		createInfo.subresourceRange.levelCount = 1;
		createInfo.subresourceRange.baseArrayLayer = 0;
		createInfo.subresourceRange.layerCount = 1;
		vkCreateImageView(&*device, &createInfo, nullptr, &swapChainImageViews[i]);
	}

	auto vert_shader = std::move(
		createShaderModule(&*device, "shaders/vert.spv").value()
	);
	auto vertex_stage = createPipelineStage(&*vert_shader, VK_SHADER_STAGE_VERTEX_BIT, "main");
	auto frag_shader = std::move(
		createShaderModule(&*device, "shaders/frag.spv").value()
	);
	auto fragment_stage = createPipelineStage(&*frag_shader, VK_SHADER_STAGE_FRAGMENT_BIT, "main");
	auto vert_input_stage = createVertexInput();
	auto input_assembly_stage = createVertexAssembly();
	auto viewport_stage = createViewportState(gpu0, &*surface);
	auto rasterizer_stage = createRasterizer();
	auto multisampling_stage = createMultisampling();
	auto color_blend_stage = createColorBlend();
	
	vert v;
	VkMemoryAllocateInfo alloc_info;
	v.config(alloc_info);
	auto uniform_mem = valloc(gpu0, &*device, alloc_info);

	auto pipeline_info = make_pipeline_info();	
	v.config(&*device, pipeline_info, uniform_mem);

	auto pipeline_layout = std::move(
		createPipelineLayout(&*device)
		.value()
	);
	
	auto render_passess = std::move(
		createRenderPass(gpu0, &*surface, &*device)
		.value()
	);

	Boy01_Head_GeoMesh h{ vert_input_stage };
	h.load(gpu0, &*device, "models/Boy01_Head_GeoMesh");

	auto pipeline = std::move(
		createPipeline(
			&*device,
			vertex_stage, fragment_stage,
			vert_input_stage,
			input_assembly_stage,
			viewport_stage,
			rasterizer_stage,
			multisampling_stage,
			color_blend_stage,
			&*pipeline_layout,
			&*render_passess).value()
	);

	std::vector<VkFramebuffer> swapChainFramebuffers(swapChainImageViews.size());
	for (size_t i = 0; i < swapChainImageViews.size(); i++) {
		VkImageView attachments[] = {
			swapChainImageViews[i]
		};

		VkFramebufferCreateInfo framebufferInfo = {};
		framebufferInfo.sType = VK_STRUCTURE_TYPE_FRAMEBUFFER_CREATE_INFO;
		framebufferInfo.renderPass = &*render_passess;;
		framebufferInfo.attachmentCount = 1;
		framebufferInfo.pAttachments = attachments;
		framebufferInfo.width = extent.currentExtent.width;
		framebufferInfo.height = extent.currentExtent.height;
		framebufferInfo.layers = 1;

		vkCreateFramebuffer(&*device, &framebufferInfo, nullptr, &swapChainFramebuffers[i]);
	}




	VkCommandPool commandPool;
	VkCommandPoolCreateInfo poolInfo = {};
	poolInfo.sType = VK_STRUCTURE_TYPE_COMMAND_POOL_CREATE_INFO;
	poolInfo.queueFamilyIndex = family_index;
	poolInfo.flags = 0; // Optional
	vkCreateCommandPool(&*device, &poolInfo, nullptr, &commandPool);

	std::vector<VkCommandBuffer> commandBuffers(32);
	VkCommandBufferAllocateInfo allocInfo = {};
	allocInfo.sType = VK_STRUCTURE_TYPE_COMMAND_BUFFER_ALLOCATE_INFO;
	allocInfo.commandPool = commandPool;
	allocInfo.level = VK_COMMAND_BUFFER_LEVEL_PRIMARY;
	allocInfo.commandBufferCount = (uint32_t)commandBuffers.size();

	vkAllocateCommandBuffers(&*device, &allocInfo, commandBuffers.data());
	for (size_t i = 0; i < swapChainImageViews.size(); i++) {
		VkCommandBufferBeginInfo beginInfo = {};
		beginInfo.sType = VK_STRUCTURE_TYPE_COMMAND_BUFFER_BEGIN_INFO;
		beginInfo.flags = 0; // Optional
		beginInfo.pInheritanceInfo = nullptr; // Optional
		vkBeginCommandBuffer(commandBuffers[i], &beginInfo);

		VkRenderPassBeginInfo renderPassInfo = {};
		renderPassInfo.sType = VK_STRUCTURE_TYPE_RENDER_PASS_BEGIN_INFO;
		renderPassInfo.renderPass = &*render_passess;
		renderPassInfo.framebuffer = swapChainFramebuffers[i];
		renderPassInfo.renderArea.offset = { 0, 0 };
		renderPassInfo.renderArea.extent = extent.currentExtent;
		VkClearValue clearColor = { 0.0f, 0.0f, 0.0f, 1.0f };
		renderPassInfo.clearValueCount = 1;
		renderPassInfo.pClearValues = &clearColor;
		
		vkCmdBeginRenderPass(commandBuffers[i], &renderPassInfo, VK_SUBPASS_CONTENTS_INLINE);
		vkCmdBindPipeline(commandBuffers[i], VK_PIPELINE_BIND_POINT_GRAPHICS, &*pipeline);
		h.draw(commandBuffers[i]);
		vkCmdEndRenderPass(commandBuffers[i]);
		vkEndCommandBuffer(commandBuffers[i]);
	}

	const int MAX_FRAMES_IN_FLIGHT = 2;


	std::vector<VkSemaphore> imageAvailableSemaphores(MAX_FRAMES_IN_FLIGHT);
	std::vector<VkSemaphore> renderFinishedSemaphores(MAX_FRAMES_IN_FLIGHT);

	std::vector<VkFence> inFlightFences(MAX_FRAMES_IN_FLIGHT);
	std::vector<VkFence> imagesInFlight(swapChainImages.size(), VK_NULL_HANDLE);

	VkFenceCreateInfo fenceInfo = {};
	fenceInfo.sType = VK_STRUCTURE_TYPE_FENCE_CREATE_INFO;
	fenceInfo.flags = VK_FENCE_CREATE_SIGNALED_BIT;

	VkSemaphoreCreateInfo semaphoreInfo = {};
	semaphoreInfo.sType = VK_STRUCTURE_TYPE_SEMAPHORE_CREATE_INFO;

	for (int i = 0; i < inFlightFences.size(); ++i)
	{
		vkCreateSemaphore(&*device, &semaphoreInfo, nullptr, &imageAvailableSemaphores[i]);
		vkCreateSemaphore(&*device, &semaphoreInfo, nullptr, &renderFinishedSemaphores[i]);
		vkCreateFence(&*device, &fenceInfo, nullptr, &inFlightFences[i]);
	}

	

	size_t currentFrame = 0;

	while (true)
	{
		vkWaitForFences(&*device, 1, &inFlightFences[currentFrame], VK_TRUE, UINT64_MAX);
		auto imgIndex = waitAcquireNextImage(&*device, &*swapChain, imageAvailableSemaphores[currentFrame]);
		if (imagesInFlight[imgIndex] != VK_NULL_HANDLE) {
			vkWaitForFences(&*device, 1, &imagesInFlight[imgIndex], VK_TRUE, UINT64_MAX);
		}
		imagesInFlight[imgIndex] = inFlightFences[currentFrame];

		VkSubmitInfo submitInfo = {};
		submitInfo.sType = VK_STRUCTURE_TYPE_SUBMIT_INFO;

		VkSemaphore waitSemaphores[] = { imageAvailableSemaphores[currentFrame] };
		VkPipelineStageFlags waitStages[] = { VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT };
		submitInfo.waitSemaphoreCount = 1;
		submitInfo.pWaitSemaphores = waitSemaphores;
		submitInfo.pWaitDstStageMask = waitStages;

		submitInfo.commandBufferCount = 1;
		submitInfo.pCommandBuffers = &commandBuffers[imgIndex];

		VkSemaphore signalSemaphores[] = { renderFinishedSemaphores[currentFrame] };
		submitInfo.signalSemaphoreCount = 1;
		submitInfo.pSignalSemaphores = signalSemaphores;

		vkResetFences(&*device, 1, &inFlightFences[currentFrame]);

		vkQueueSubmit(queue, 1, &submitInfo, inFlightFences[currentFrame]);
		
		queue << present(&*swapChain, imgIndex, renderFinishedSemaphores[currentFrame]);

		currentFrame = (currentFrame + 1) % MAX_FRAMES_IN_FLIGHT;
	}

	return 0;
}

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

int WinMain(HINSTANCE hInstance,
	HINSTANCE hPrevInstance,
	LPSTR lpCmdLine, int nCmdShow)
{
	const wchar_t className[] = L"windowClass";

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
	wc.lpszClassName = className;
	wc.hIconSm = LoadIcon(NULL, IDI_APPLICATION);
	RegisterClassExW(&wc);

	HWND hWnd = CreateWindowExW(
		WS_EX_CLIENTEDGE,
		className,
		L"Game",
		WS_OVERLAPPEDWINDOW,
		CW_USEDEFAULT, CW_USEDEFAULT,
		640, 480,
		NULL, NULL, hInstance, NULL);
	ShowWindow(hWnd, nCmdShow);
	UpdateWindow(hWnd);

	LoopThreadParams p{ hInstance, hWnd };
	DWORD   dwThreadIdArray;
	HANDLE  hThreadArray = CreateThread(
		NULL,
		0,
		LoopThread,
		&p,
		0,
		&dwThreadIdArray);


	MSG msg;
	while (GetMessage(&msg, NULL, 0, 0))
	{
		TranslateMessage(&msg);
		DispatchMessage(&msg);
	}

	DestroyWindow(hWnd);
	return 0;
}