#version 400

subroutine float lightRoutine();

out vec4 fragColor;

in vec2 texCoord;
in vec4 shadowCoords;
in vec3 lightDirEyeSpace;
in vec3 lightEyeDirTangentSpace;
in vec3 lightEyeDirEyeSpace;
in vec3 wp;
in vec3 norm;
in vec3 lightDir;

uniform sampler2D tex;
uniform sampler2D depthMap;
uniform sampler2D nmap;
subroutine uniform lightRoutine lightContribution;
uniform vec4 mean;

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

const float minLight = 0.003;

const float lightSize = 0.01;
const float c = 10.0f;
const float shadowMapSize = 512.0f;
const float pixel = 1.0f / shadowMapSize;
const float esmEpsilon = 0.001;
const float depthBiasFactor = 1.2;

vec4 sampleSat(vec2 uv, float size) {
  size = max(size, 1);
  vec4 sum =
            textureLod(depthMap, uv, 0)
          - textureLod(depthMap, uv + size * vec2(pixel, 0), 0)
          - textureLod(depthMap, uv + size * vec2(0, pixel), 0)
          + textureLod(depthMap, uv + size * vec2(pixel, pixel), 0);

  return (sum / (size * size)) + mean;
}

void main(void) {
  vec3 fragToLight = normalize(lightEyeDirTangentSpace);
  vec3 normal = normalize(texture(nmap, texCoord).rgb * 2.0 - 1.0);

  float d = length(lightEyeDirEyeSpace);
  float b = 1.0 / (4 * light.radius * light.radius * minLight);
  float att = 1 + b * d * d;
  float energy = clamp(dot(normal, fragToLight), 0, 1) / att;

  vec3 diffuse = texture(tex, texCoord).rgb;
  fragColor = vec4(lightContribution() * diffuse * light.color * energy, 1);
}

////////////////////////////////////////////////////////////////////////////////


float estimateBlockerDepth (vec2 shadowCoords, float depth, float searchWidth) {
  shadowCoords -= vec2(searchWidth);

  vec2 sums = sampleSat(shadowCoords, 2 * searchWidth * shadowMapSize).rg;

  float ed = exp(-c * (depth * depthBiasFactor));
  float esm = ed * sums.x;
  return ed * sums.y / (1 - esm);
}

float estimatePenumbraWidth(vec2 shadowCoords, float depth, float blockerDepth) {
  return (depth - blockerDepth) * lightSize / blockerDepth;
}

float esmDepthTest(vec2 uv, float depth, float filterWidth, out bool failed) {
  uv -= vec2(filterWidth);

  float esm = sampleSat(uv, 2 * filterWidth * shadowMapSize).x;
  float esmTest = esm * exp(-c * (depth * depthBiasFactor));
  failed = esmTest >= 1 + esmEpsilon;
  return esmTest;
}

subroutine (lightRoutine)
float spotlight() {
  float theta = dot(lightDirEyeSpace, -normalize(lightEyeDirEyeSpace));
  if (theta >= spotlightParams.cosConeRadius) {
    vec3 shadowDiv = shadowCoords.xyz / shadowCoords.w;
    float near = 1.0f;
    float far = 100.0f;
    float ourDepth = (length(wp - light.pos) - near) / (far - near);

    bool blocked;
    float blockerSearchSize = lightSize * ourDepth;
    float blockerDepth = estimateBlockerDepth(shadowDiv.xy, ourDepth, blockerSearchSize);

    float visibility = 1.0f;
    float penumbra = estimatePenumbraWidth(shadowDiv.xy, ourDepth, blockerDepth);
    penumbra = ((ourDepth - blockerDepth) * blockerSearchSize) / blockerDepth;
    bool esmFailed;
    visibility = esmDepthTest(shadowDiv.xy, ourDepth, min(penumbra, blockerSearchSize), esmFailed);

    if (esmFailed) visibility = 1;

    return clamp(visibility, 0, 1) * smoothstep(0, 1, (theta - spotlightParams.cosConeRadius) / spotlightParams.cosPenumbraRadius);
  }
  else {
    return 0;
  }
}

////////////////////////////////////////////////////////////////////////////////

subroutine (lightRoutine)

float omni() {
  return 1;
}
