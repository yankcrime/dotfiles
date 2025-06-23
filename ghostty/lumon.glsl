float random(vec2 st) {
    return fract(sin(dot(st.xy, vec2(12.9898,78.233))) * 43758.5453123);
}

void mainImage(out vec4 fragColor, in vec2 fragCoord)
{
    vec2 uv = fragCoord.xy / iResolution.xy;

    // Aspect-corrected center
    vec2 centeredUV = uv - 0.5;
    centeredUV.x *= iResolution.x / iResolution.y;

    // Tuned gradient colors: darker center, softer edge
    vec3 gradientStartColor = vec3(0.015, 0.12, 0.19);  // muted teal-blue center
    vec3 gradientEndColor   = vec3(0.015, 0.08, 0.13);  // lifted edge tone

    float dist = length(centeredUV);
    float mixValue = smoothstep(0.0, 0.7, dist);
    vec3 gradientColor = mix(gradientStartColor, gradientEndColor, mixValue);

    // Subtle grainy noise
    float noise = (random(fragCoord * 0.5) - 0.5) * 0.02;
    gradientColor += noise;

    // Sample terminal texture
    vec4 terminalColor = texture(iChannel0, uv);

    // Mask for terminal content blending
    float mask = 1.0 - step(0.5, dot(terminalColor.rgb, vec3(1.0)));
    vec3 blendedColor = mix(terminalColor.rgb, gradientColor, mask);
    vec3 fontColor = vec3(0.68, 0.78, 0.90);

    fragColor = vec4(blendedColor, terminalColor.a);
}
