const fs = require('fs');
const path = require('path');

const file = process.argv[2];
const className = file.replace(path.dirname(file)+"\\","").replace('.glsl', "");
const txt = fs.readFileSync(file);

function* all_matches(regex, str)
{
    var m;
    while (m = regex.exec(str)) {
        yield m;
    }
}

// bindingDescription.binding = 0;
// bindingDescription.stride = sizeof(Vertex);
// bindingDescription.inputRate = VK_VERTEX_INPUT_RATE_VERTEX;

// VkVertexInputAttributeDescription atts[];
// atts[0].binding = 0;
// atts[0].location = 0;
// atts[0].format = VK_FORMAT_R32G32_SFLOAT;
// atts[0].offset = offsetof(Vertex, pos);

// let i = 0;
// const layouts = /layout\s*\(\s*location\s*=\s*(?<POS>\d+)\s*\)\s+(?<DIR>in|out)\s+(?<TYPE>.+?)\s+(.+?);/g
// Array.from(all_matches(layouts, txt))
//     .filter(x => x.groups.DIR == "in")    
//     .forEach(x => {
//         if(x.groups.TYPE == "vec2") {
//             console.log(`atts[${i}].format = VK_FORMAT_R32G32_SFLOAT;`);
//         } else if(x.groups.TYPE == "vec3") {
//             console.log(`atts[${i}].format = VK_FORMAT_R32G32B32_SFLOAT;`);
//         } else if(x.groups.TYPE == "vec4") {
//             console.log(`atts[${i}].format = VK_FORMAT_R32G32B32A32_SFLOAT;`);
//         }
//         ++i;
//     });

let code = [];
// uniforms
const uniforms = /layout\s*\(\s*binding\s*=\s*(?<POS>\d+)\s*\)\s+uniform\s+(?<NAME>.+?)\s*\{(?<DECL>(\s*(.+?)\s+(.+?)\s*;)+)\s*}\s*(.+?);/gm;
var m = uniforms.exec(txt);

const ui = 0;
//console.log(m.groups.DECL);
code.push(`#include <memory>\n`);
code.push(`#include <vulkan/vulkan.h>\n`);
code.push(`class ${className}\n`);
code.push(`{\n`);
code.push(`   VkDescriptorSetLayout descriptorSetLayout;\n`);
code.push(`   VkDeviceMemory* mem;\n`);
code.push(`   VkDevice* dev;\n`);
code.push(`   VkDescriptorSetLayout layout;\n`);
code.push(`public:\n`);
code.push(`   void config(VkDevice& device, VkPipelineLayoutCreateInfo& info, VkDeviceMemory& memory)\n`);
code.push(`   {\n`);
code.push(`       dev = &device;\n`);
code.push(`       mem = &memory;\n`);
code.push(`\n`);
code.push(`       VkDescriptorSetLayoutBinding binding = {};\n`);
code.push(`       binding.binding = ${ui};\n`);
code.push(`       binding.descriptorType = VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER;\n`);
code.push(`       binding.descriptorCount = 1;\n`);
code.push(`       binding.stageFlags = VK_SHADER_STAGE_VERTEX_BIT;\n`);
code.push(`       binding.pImmutableSamplers = nullptr;\n`);
code.push(`\n`);
code.push(`       VkDescriptorSetLayoutCreateInfo layout_info = {};\n`);
code.push(`       layout_info.sType = VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_CREATE_INFO;\n`);
code.push(`       layout_info.bindingCount = 1;\n`);
code.push(`       layout_info.pBindings = &binding;\n`);
code.push(`       vkCreateDescriptorSetLayout(device, &layout_info, nullptr, &layout);\n`);
code.push(`\n`);
code.push(`       info.setLayoutCount = 1;\n`);
code.push(`       info.pSetLayouts = &layout;\n`);
code.push(`   }\n\n`);
let vars = [];
let types = [];
const parameters = [];
let totalSize = 0;
const fields = /(\s*(?<NAME>.+?)\s+(?<VALUE>.+?)\s*;)/gm;
Array.from(all_matches(fields, m.groups.DECL))
    .forEach(x => {
        vars.push(x.groups.VALUE);
        types.push(x.groups.NAME);
        parameters.push(`const ${x.groups.NAME}& ${x.groups.VALUE}`);

        const floatsize = 4;
        if(x.groups.NAME == "vec2") totalSize += 2 * floatsize;
        if(x.groups.NAME == "vec3") totalSize += 3 * floatsize;
        if(x.groups.NAME == "vec4") totalSize += 4 * floatsize;

        if(x.groups.NAME == "mat3") totalSize += 9 * floatsize;
        if(x.groups.NAME == "mat4") totalSize += 16 * floatsize;
    })

const memcpys = vars.map(x => `       memcpy((void*)data, &item.${x}, sizeof(item.${x}));\n       data += sizeof(item.${x});\n`).join("");
const sizeofs = vars.map(x => `sizeof(${x})`).join(" + ");
code.push(`   template <typename T>`);
code.push(`   void set(const T& item)\n`);
code.push(`   {\n`);
code.push(`       uint8_t* data;\n`);
code.push(`       vkMapMemory(*dev, *mem, 0, ${sizeofs}, 0, &data);\n`);
code.push(`${memcpys}       vkUnmapMemory(*dev, *mem);\n`);
code.push(`   }\n`);
code.push(`\n`);
code.push(`   void config(VkMemoryAllocateInfo& info)\n`);
code.push(`   {\n`);
code.push(`       info.allocationSize = ${totalSize};\n`);
code.push(`   }\n`);
code.push(`};\n`);

fs.writeFileSync(file.replace(".glsl", ".h"), code.join(""));