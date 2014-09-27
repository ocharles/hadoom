#version 330

out vec4 fragColor;

in vec3 norm;
in vec3 worldPos;
in vec2 texCoord;
in vec4 shadowCoords;
in vec3 lightDirEyeSpace;
in vec3 lightPosEye;

uniform sampler2D tex;
uniform sampler2DShadow depthMap;

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
  vec3 dir = lightPosEye - worldPos;
  float d = length(dir);
  vec3 l = dir / d;
  if (dot(lightDirEyeSpace, -l) > 0.79) {
    float b = 1.0 / (light.radius * light.radius * minLight);
    float att = 1 + b * d * d;

    float visibility = texture(depthMap, shadowCoords.xyz / shadowCoords.w);
    vec3 diffuse = texture2D(tex, texCoord).rgb;
    fragColor += vec4(visibility * diffuse * light.color * clamp(dot(norm, l), 0, 1) / att, 1);
  }
}
