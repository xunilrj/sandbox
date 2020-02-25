#include <memory>
#include <vulkan/vulkan.h>
class vert
{
   VkDescriptorSetLayout descriptorSetLayout;
   VkDeviceMemory* mem;
   VkDevice* dev;
   VkDescriptorSetLayout layout;
public:
   void config(VkDevice& device, VkPipelineLayoutCreateInfo& info, VkDeviceMemory& memory)
   {
       dev = &device;
       mem = &memory;

       VkDescriptorSetLayoutBinding binding = {};
       binding.binding = 0;
       binding.descriptorType = VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER;
       binding.descriptorCount = 1;
       binding.stageFlags = VK_SHADER_STAGE_VERTEX_BIT;
       binding.pImmutableSamplers = nullptr;

       VkDescriptorSetLayoutCreateInfo layout_info = {};
       layout_info.sType = VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_CREATE_INFO;
       layout_info.bindingCount = 1;
       layout_info.pBindings = &binding;
       vkCreateDescriptorSetLayout(device, &layout_info, nullptr, &layout);

       info.setLayoutCount = 1;
       info.pSetLayouts = &layout;
   }

   template <typename T>   void set(const T& item)
   {
       uint8_t* data;
       vkMapMemory(*dev, *mem, 0, sizeof(mvp) + sizeof(mvp2), 0, &data);
       memcpy((void*)data, &item.mvp, sizeof(item.mvp));
       data += sizeof(item.mvp);
       memcpy((void*)data, &item.mvp2, sizeof(item.mvp2));
       data += sizeof(item.mvp2);
       vkUnmapMemory(*dev, *mem);
   }

   void config(VkMemoryAllocateInfo& info)
   {
       info.allocationSize = 128;
   }
};
