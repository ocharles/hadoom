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
out vec3 _norm;
out vec3 wp;

struct LightInfo {
  vec3 pos;
  vec3 color;
  float radius;
};

struct SpotlightInfo {
  vec3 direction;
  float cosConeRadius;
  float cosPenumbraRadius;
};

layout(std140) uniform Light {
  LightInfo light;
  SpotlightInfo spotlightParams;
};

void main(void) {
  wp = in_Position;
  vec4 vp = view * vec4(in_Position, 1);
  vec3 worldPos = vp.xyz;
  texCoord = in_UV;
  shadowCoords = bias * lightProjection * camV * vec4(in_Position, 1);
  lightDirEyeSpace = (view * vec4(spotlightParams.direction, 0)).xyz;
  vec3 lightPosEye = (view * vec4(light.pos, 1)).xyz;
  gl_Position = projection * vp;

  mat3 tbn = transpose(
    mat3(
     (view * vec4(in_Tangent, 0)).xyz,
     (view * vec4(in_Bitangent, 0)).xyz,
     (view * vec4(in_Normal, 0)).xyz
    )
  );

  lightEyeDirEyeSpace = lightPosEye - worldPos;
  lightEyeDirTangentSpace = tbn * lightEyeDirEyeSpace;

  _norm = in_Normal;
}
