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
uniform usampler2D depthMap;
uniform sampler2D nmap;
subroutine uniform lightRoutine lightContribution;

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

void main(void) {
  vec3 fragToLight = normalize(lightEyeDirTangentSpace);
  vec3 normal = normalize(texture2D(nmap, texCoord).rgb * 2.0 - 1.0);

  float d = length(lightEyeDirEyeSpace);
  float b = 1.0 / (4 * light.radius * light.radius * minLight);
  float att = 1 + b * d * d;
  float energy = clamp(dot(normal, fragToLight), 0, 1) / att;

  vec3 diffuse = texture2D(tex, texCoord).rgb;
  fragColor = vec4(lightContribution() * diffuse * light.color * energy, 1);
}

////////////////////////////////////////////////////////////////////////////////

const float blockerSearchSamples = 6;
const float pcfSamples = 10;
const float scale = 1073741824;
const float lightSize = 0.03;
const float c = 88.0f;
const float shadowMapSize = 2048.0f;
const float pixel = 1.0f / shadowMapSize;
const float esmEpsilon = 0.001;
const float depthBiasFactor = 1.15;

float estimateBlockerDepth (vec2 shadowCoords, float depth, float searchWidth, out bool blocked) {
  float stepSize = 2 * searchWidth / blockerSearchSamples;

  shadowCoords -= vec2(searchWidth);

  float blockerSum = 0;
  float blockerCount = 0;

  for (int i = 0; i < blockerSearchSamples; i++) {
    for (int j = 0; j < blockerSearchSamples; j++) {
      float blockerDepth = texture(depthMap, shadowCoords + vec2(i, j) * stepSize) / scale;
      if (blockerDepth < depth) {
        blockerSum += blockerDepth;
        blockerCount++;
        blocked = true;
      }
    }
  }

  return blockerSum / blockerCount;
}

float estimatePenumbraWidth(vec2 shadowCoords, float depth, float blockerDepth) {
  return (depth - blockerDepth) * lightSize / blockerDepth;
}

float esmDepthTest(vec2 uv, float depth, float filterWidth, out bool failed) {
  float stepSize = 2 * filterWidth / pcfSamples;
  uv -= vec2(filterWidth);

  float esm = 0;
  for (int i = 0; i < pcfSamples; i++) {
    for (int j = 0; j < pcfSamples; j++) {
      float tl = texture(depthMap, uv + vec2(i, j) * stepSize) / scale;
      float tr = texture(depthMap, uv + vec2(i, j) * stepSize + vec2(pixel, 0)) / scale;
      float bl = texture(depthMap, uv + vec2(i, j) * stepSize + vec2(0, pixel)) / scale;
      float br = texture(depthMap, uv + vec2(i, j) * stepSize + vec2(pixel, pixel)) / scale;
      vec2 f = fract((uv + vec2(i, j) * stepSize) * shadowMapSize);

      esm += mix(mix(exp(tl * c), exp(tr * c), f.x),
                 mix(exp(bl * c), exp(br * c), f.x),
                 f.y);
    }
  }

  float esmTest = (esm / (pcfSamples * pcfSamples)) * exp(-c * (depth * depthBiasFactor));

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
    float blockerDepth = estimateBlockerDepth(shadowDiv.xy, ourDepth, blockerSearchSize, blocked);

    float visibility = 1.0f;
    if(blocked) {
      float penumbra = estimatePenumbraWidth(shadowDiv.xy, ourDepth, blockerDepth);
      penumbra = ((ourDepth - blockerDepth) * blockerSearchSize) / blockerDepth;
      bool esmFailed;
      visibility = esmDepthTest(shadowDiv.xy, ourDepth, min(penumbra, blockerSearchSize), esmFailed);

      if (esmFailed) visibility = 1;
    }

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
