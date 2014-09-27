#version 330

out vec4 fragColor;

in vec2 texCoord;
in vec4 shadowCoords;
in vec3 lightDirEyeSpace;
in vec3 lightEyeDirTangentSpace;
in vec3 lightEyeDirEyeSpace;

uniform sampler2D tex;
uniform sampler2DShadow depthMap;
uniform sampler2D nmap;

struct LightInfo {
  vec3 pos;
  vec3 color;
  vec3 direction;
  float radius;
};
layout(std140) uniform Light {
  LightInfo light;
};

const float minLight = 0.01;

void main(void) {
  vec3 l = normalize(lightEyeDirTangentSpace);
  float d = length(lightEyeDirEyeSpace);

  vec3 textureNormal = normalize(texture2D(nmap, texCoord).rgb * 2.0 - 1.0);
  if (dot(lightDirEyeSpace, -normalize(lightEyeDirEyeSpace)) > 0.5) {
    vec3 diffuse = texture2D(tex, texCoord).rgb;
    float b = 1.0 / (light.radius * light.radius * minLight);
    float att = 1 + b * d * d;
    float visibility = texture(depthMap, shadowCoords.xyz / shadowCoords.w);
    fragColor += vec4(visibility * diffuse * light.color * clamp(dot(textureNormal, l), 0, 1) / att, 1);
  }
}
