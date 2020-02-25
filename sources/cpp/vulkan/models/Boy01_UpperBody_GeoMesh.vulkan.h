#include <array>
#include <fstream>
#include <vulkan/vulkan.h>
class Boy01_UpperBody_GeoMesh
{
    std::array<VkVertexInputAttributeDescription, 3> atts = {};
    VkVertexInputBindingDescription desc;
    VkBuffer buffer;
    VkDeviceMemory vertexBufferMemory;
    VkDeviceSize vertex_buffer_offset;
    VkDeviceSize index_buffer_offset;
public:
    Boy01_UpperBody_GeoMesh(VkPipelineVertexInputStateCreateInfo& vi_info)
    {
        atts[0] = {};
        atts[0].binding = 0;
        atts[0].location = 0;
        atts[0].format = VK_FORMAT_R32G32B32_SFLOAT;
        atts[0].offset = 0;
        atts[1] = {};
        atts[1].binding = 0;
        atts[1].location = 1;
        atts[1].format = VK_FORMAT_R32G32B32_SFLOAT;
        atts[1].offset = 12;
        atts[2] = {};
        atts[2].binding = 0;
        atts[2].location = 2;
        atts[2].format = VK_FORMAT_R32G32_SFLOAT;
        atts[2].offset = 24;
        desc.binding = {};
        desc.binding = 0;
        desc.stride = 32;
        desc.inputRate = VK_VERTEX_INPUT_RATE_VERTEX;
        vi_info.vertexBindingDescriptionCount = 1;
        vi_info.pVertexBindingDescriptions = &desc;
        vi_info.vertexAttributeDescriptionCount = 3;
        vi_info.pVertexAttributeDescriptions = atts.data();
    }
    uint32_t findMemoryType(const VkPhysicalDevice& gpu, uint32_t typeFilter, VkMemoryPropertyFlags properties) {
        VkPhysicalDeviceMemoryProperties memProperties;
        vkGetPhysicalDeviceMemoryProperties(gpu, &memProperties);
        for (uint32_t i = 0; i < memProperties.memoryTypeCount; i++) {
            if ((typeFilter & (1 << i)) && (memProperties.memoryTypes[i].propertyFlags & properties) == properties) {
                return i;
            }
        }
    }
    bool load(const VkPhysicalDevice& gpu, const VkDevice& device, const std::string& path, size_t max_size = std::numeric_limits<size_t>::max())
    {
        std::ifstream file_vertex{ path, std::ios::ate | std::ios::binary };
        if (!file_vertex.is_open()) return false;
        auto pos = file_vertex.tellg();
        if (pos == -1) return false;
        auto vertex_file_size = (size_t)pos;
        if (vertex_file_size > max_size) return false;

        std::ifstream file_index{ path + ".index", std::ios::ate | std::ios::binary };
        if (!file_index.is_open()) return false;
        pos = file_index.tellg();
        if (pos == -1) return false;
        auto index_file_size = (size_t)pos;
        if (index_file_size > max_size) return false;

        VkBufferCreateInfo bufferInfo = {};
        bufferInfo.sType = VK_STRUCTURE_TYPE_BUFFER_CREATE_INFO;
        bufferInfo.size = vertex_file_size + index_file_size;
        bufferInfo.usage = VK_BUFFER_USAGE_VERTEX_BUFFER_BIT | VK_BUFFER_USAGE_INDEX_BUFFER_BIT;
        bufferInfo.sharingMode = VK_SHARING_MODE_EXCLUSIVE;
        auto r = vkCreateBuffer(device, &bufferInfo, nullptr, &buffer);

        VkMemoryAllocateInfo allocInfo = {};
        void* data;
        VkMemoryRequirements memRequirements;
        vkGetBufferMemoryRequirements(device, buffer, &memRequirements);
        allocInfo.sType = VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_INFO;
        allocInfo.allocationSize = memRequirements.size;
        allocInfo.memoryTypeIndex = findMemoryType(gpu, memRequirements.memoryTypeBits, VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT | VK_MEMORY_PROPERTY_HOST_COHERENT_BIT);
        r = vkAllocateMemory(device, &allocInfo, nullptr, &vertexBufferMemory);
        r = vkBindBufferMemory(device, buffer, vertexBufferMemory, 0);

        r = vkMapMemory(device, vertexBufferMemory, 0, vertex_file_size, 0, &data);
        file_vertex.seekg(0).read((char*)data, vertex_file_size);
        file_vertex.close();
        vkUnmapMemory(device, vertexBufferMemory);
        vertex_buffer_offset = 0;

        r = vkMapMemory(device, vertexBufferMemory, vertex_file_size, index_file_size, 0, &data);
        file_index.seekg(0).read((char*)data, index_file_size);
        file_index.close();
        vkUnmapMemory(device, vertexBufferMemory);
        index_buffer_offset = vertex_file_size;

        return true;
    }
    void draw(const VkCommandBuffer& command_buffer)
    {
        vkCmdBindVertexBuffers(command_buffer, 0, 1, &buffer, &vertex_buffer_offset);
        vkCmdBindIndexBuffer(command_buffer, buffer, index_buffer_offset, VK_INDEX_TYPE_UINT16);
        vkCmdDrawIndexed(command_buffer, 4140, 1, 0, 0, 0);
    }
};
