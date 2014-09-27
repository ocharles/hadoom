#version 330

uniform mat4 projection;
uniform mat4 lightProjection;
uniform mat4 view;

// Camera tings
uniform mat4 camV;
uniform mat4 bias;

in vec3 in_Position;
in vec3 in_Normal;
in vec3 in_Tangent;
in vec3 in_Bitangent;
in vec2 in_UV;

out vec2 texCoord;
out vec4 shadowCoords;
out vec3 lightDirEyeSpace;
out vec3 lightEyeDirTangentSpace;
out vec3 lightEyeDirEyeSpace;

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
  vec3 worldPos = wp.xyz;
  texCoord = in_UV;
  shadowCoords = bias * lightProjection * camV * vec4(in_Position, 1);
  lightDirEyeSpace = (view * vec4(light.direction, 0)).xyz;
  vec3 lightPosEye = (view * vec4(light.pos, 1)).xyz;
  gl_Position = projection * view * vec4(in_Position, 1);

  mat3 tbn = transpose(
    mat3(
     (view * vec4(in_Tangent, 0)).xyz,
     (view * vec4(in_Bitangent, 0)).xyz,
     (view * vec4(in_Normal, 0)).xyz
    )
  );

  lightEyeDirEyeSpace = lightPosEye - worldPos;
  lightEyeDirTangentSpace = tbn * lightEyeDirEyeSpace;
}
