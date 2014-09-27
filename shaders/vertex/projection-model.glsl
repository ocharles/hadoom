#version 330

uniform mat4 projection;
uniform mat4 view;

// Camera tings
uniform mat4 camV;
uniform mat4 bias;

in vec3 in_Position;
in vec3 in_Normal;
in vec2 in_UV;

out vec3 norm;
out vec3 worldPos;
out vec2 texCoord;
out vec4 shadowCoords;
out vec3 lightDirEyeSpace;
out vec3 lightPosEye;

struct LightInfo {
  vec3 pos;
  vec3 color;
  vec3 direction;
  float radius;
};
layout(std140) uniform Light {
  LightInfo light;
};

void main(void) {
  vec4 wp = view * vec4(in_Position, 1);
  norm = (view * vec4(in_Normal, 0)).xyz;
  worldPos = wp.xyz;
  texCoord = in_UV;
  shadowCoords = bias * projection * camV * vec4(in_Position, 1);
  lightDirEyeSpace = (view * vec4(light.direction, 0)).xyz;
  lightPosEye = (view * vec4(light.pos, 1)).xyz;
  gl_Position = projection * view * vec4(in_Position, 1);
}
