// Made with Amplify Shader Editor
// Available at the Unity Asset Store - http://u3d.as/y3X 
Shader "ASESampleShaders/SRP Lightweight/GlintSparkle"
{
    Properties
    {
		_Albedo("Albedo", Color) = (0,0,0,0)
		_Normals("Normals", 2D) = "bump" {}
		[Header(Glint Effect)]_GlintColor("Color", Color) = (0,0,0,0)
		[KeywordEnum(X,Y,Z)] _Direction("Direction", Float) = 1
		[Toggle(_INVERTDIRECTION_ON)] _InvertDirection("Invert Direction", Float) = 1
		_SizeSpeedInterval("Size Speed Interval", Vector) = (1,1,1,0)
		_GlintFresnel("Fresnel Bias, Scale, Power", Vector) = (0,0,0,0)
		_TailHeadFalloff("Tail Head Falloff", Range( 0 , 1)) = 0.5
		_Brightness("Brightness", Float) = 1
		[Space(10)][Header(Sparkles)]_SparkleColor("Color", Color) = (0,0,0,0)
		[NoScaleOffset]_Noise("Noise", 2D) = "white" {}
		_Frequency("Frequency", Range( 0 , 100)) = 20
		_Threshold("Threshold", Range( 0 , 1)) = 0.5
		_Range("Range", Range( 0 , 1)) = 0
		_SparklesBrightness("Brightness", Float) = 2
		_SpakleSpeed("Spakle Speed", Range( 0 , 0.1)) = 0
		_ScreenContribution("Screen Contribution", Range( 0 , 1)) = 0.2
		_SparkleFresnel("Fresnel Bias, Scale, Power", Vector) = (0,0,0,0)
		[Header(Body Glow)]_BodyGlow("Color", Color) = (0,0,0,0)
		_MainGlowFresnel("Fresnel Bias, Scale, Power", Vector) = (0,0,0,0)
		[HideInInspector] _texcoord( "", 2D ) = "white" {}
    }

    SubShader
    {
		
        Tags { "RenderPipeline"="LightweightPipeline" "RenderType"="Opaque" "Queue"="Geometry" }

		Cull Back
		HLSLINCLUDE
		#pragma target 3.0
		ENDHLSL
		
        Pass
        {
			
        	Tags { "LightMode"="LightweightForward" }

        	Name "Base"
			Blend One Zero
			ZWrite On
			ZTest LEqual
			Offset 0 , 0
			ColorMask RGBA
            
        	HLSLPROGRAM
            // Required to compile gles 2.0 with standard srp library
            #pragma prefer_hlslcc gles
            #pragma exclude_renderers d3d11_9x
            

        	// -------------------------------------
            // Lightweight Pipeline keywords
            #pragma multi_compile _ _MAIN_LIGHT_SHADOWS
            #pragma multi_compile _ _MAIN_LIGHT_SHADOWS_CASCADE
            #pragma multi_compile _ _ADDITIONAL_LIGHTS_VERTEX _ADDITIONAL_LIGHTS
            #pragma multi_compile _ _ADDITIONAL_LIGHT_SHADOWS
            #pragma multi_compile _ _SHADOWS_SOFT
            #pragma multi_compile _ _MIXED_LIGHTING_SUBTRACTIVE
            
        	// -------------------------------------
            // Unity defined keywords
            #pragma multi_compile _ DIRLIGHTMAP_COMBINED
            #pragma multi_compile _ LIGHTMAP_ON
            #pragma multi_compile_fog

            //--------------------------------------
            // GPU Instancing
            #pragma multi_compile_instancing

            #pragma vertex vert
        	#pragma fragment frag

        	#define _NORMALMAP 1
        	#pragma shader_feature _INVERTDIRECTION_ON
        	#pragma shader_feature _DIRECTION_X _DIRECTION_Y _DIRECTION_Z


        	#include "Packages/com.unity.render-pipelines.lightweight/ShaderLibrary/Core.hlsl"
        	#include "Packages/com.unity.render-pipelines.lightweight/ShaderLibrary/Lighting.hlsl"
        	#include "Packages/com.unity.render-pipelines.core/ShaderLibrary/Color.hlsl"
        	#include "Packages/com.unity.render-pipelines.core/ShaderLibrary/UnityInstancing.hlsl"
        	#include "Packages/com.unity.render-pipelines.lightweight/ShaderLibrary/ShaderGraphFunctions.hlsl"

            CBUFFER_START(UnityPerMaterial)
			float4 _Albedo;
			sampler2D _Normals;
			float4 _Normals_ST;
			float4 _GlintColor;
			float3 _SizeSpeedInterval;
			float _TailHeadFalloff;
			float3 _GlintFresnel;
			float _Brightness;
			float4 _SparkleColor;
			float _Threshold;
			float _Range;
			sampler2D _Noise;
			float _ScreenContribution;
			float4 _Noise_ST;
			float _Frequency;
			float _SpakleSpeed;
			float _SparklesBrightness;
			float3 _SparkleFresnel;
			float4 _BodyGlow;
			float3 _MainGlowFresnel;
			CBUFFER_END
			
			
            struct GraphVertexInput
            {
                float4 vertex : POSITION;
                float3 ase_normal : NORMAL;
                float4 ase_tangent : TANGENT;
                float4 texcoord1 : TEXCOORD1;
				float4 ase_texcoord : TEXCOORD0;
                UNITY_VERTEX_INPUT_INSTANCE_ID
            };

        	struct GraphVertexOutput
            {
                float4 clipPos                : SV_POSITION;
                float4 lightmapUVOrVertexSH	  : TEXCOORD0;
        		half4 fogFactorAndVertexLight : TEXCOORD1; // x: fogFactor, yzw: vertex light
            	float4 shadowCoord            : TEXCOORD2;
				float4 tSpace0					: TEXCOORD3;
				float4 tSpace1					: TEXCOORD4;
				float4 tSpace2					: TEXCOORD5;
				float4 ase_texcoord7 : TEXCOORD7;
				float4 ase_texcoord8 : TEXCOORD8;
				float4 ase_texcoord9 : TEXCOORD9;
                UNITY_VERTEX_INPUT_INSTANCE_ID
            	UNITY_VERTEX_OUTPUT_STEREO
            };


            GraphVertexOutput vert (GraphVertexInput v)
        	{
        		GraphVertexOutput o = (GraphVertexOutput)0;
                UNITY_SETUP_INSTANCE_ID(v);
            	UNITY_TRANSFER_INSTANCE_ID(v, o);
        		UNITY_INITIALIZE_VERTEX_OUTPUT_STEREO(o);

				float4 ase_clipPos = TransformObjectToHClip((v.vertex).xyz);
				float4 screenPos = ComputeScreenPos(ase_clipPos);
				o.ase_texcoord9 = screenPos;
				
				o.ase_texcoord7.xy = v.ase_texcoord.xy;
				o.ase_texcoord8 = v.vertex;
				
				//setting value to unused interpolator channels and avoid initialization warnings
				o.ase_texcoord7.zw = 0;
				float3 vertexValue =  float3( 0, 0, 0 ) ;
				#ifdef ASE_ABSOLUTE_VERTEX_POS
				v.vertex.xyz = vertexValue;
				#else
				v.vertex.xyz += vertexValue;
				#endif
				v.ase_normal =  v.ase_normal ;

        		// Vertex shader outputs defined by graph
                float3 lwWNormal = TransformObjectToWorldNormal(v.ase_normal);
				float3 lwWorldPos = TransformObjectToWorld(v.vertex.xyz);
				float3 lwWTangent = TransformObjectToWorldDir(v.ase_tangent.xyz);
				float3 lwWBinormal = normalize(cross(lwWNormal, lwWTangent) * v.ase_tangent.w);
				o.tSpace0 = float4(lwWTangent.x, lwWBinormal.x, lwWNormal.x, lwWorldPos.x);
				o.tSpace1 = float4(lwWTangent.y, lwWBinormal.y, lwWNormal.y, lwWorldPos.y);
				o.tSpace2 = float4(lwWTangent.z, lwWBinormal.z, lwWNormal.z, lwWorldPos.z);

                VertexPositionInputs vertexInput = GetVertexPositionInputs(v.vertex.xyz);
                
         		// We either sample GI from lightmap or SH.
        	    // Lightmap UV and vertex SH coefficients use the same interpolator ("float2 lightmapUV" for lightmap or "half3 vertexSH" for SH)
                // see DECLARE_LIGHTMAP_OR_SH macro.
        	    // The following funcions initialize the correct variable with correct data
        	    OUTPUT_LIGHTMAP_UV(v.texcoord1, unity_LightmapST, o.lightmapUVOrVertexSH.xy);
        	    OUTPUT_SH(lwWNormal, o.lightmapUVOrVertexSH.xyz);

        	    half3 vertexLight = VertexLighting(vertexInput.positionWS, lwWNormal);
        	    half fogFactor = ComputeFogFactor(vertexInput.positionCS.z);
        	    o.fogFactorAndVertexLight = half4(fogFactor, vertexLight);
        	    o.clipPos = vertexInput.positionCS;

        	#ifdef _MAIN_LIGHT_SHADOWS
        		o.shadowCoord = GetShadowCoord(vertexInput);
        	#endif
        		return o;
        	}

        	half4 frag (GraphVertexOutput IN ) : SV_Target
            {
            	UNITY_SETUP_INSTANCE_ID(IN);

        		float3 WorldSpaceNormal = normalize(float3(IN.tSpace0.z,IN.tSpace1.z,IN.tSpace2.z));
				float3 WorldSpaceTangent = float3(IN.tSpace0.x,IN.tSpace1.x,IN.tSpace2.x);
				float3 WorldSpaceBiTangent = float3(IN.tSpace0.y,IN.tSpace1.y,IN.tSpace2.y);
				float3 WorldSpacePosition = float3(IN.tSpace0.w,IN.tSpace1.w,IN.tSpace2.w);
				float3 WorldSpaceViewDirection = SafeNormalize( _WorldSpaceCameraPos.xyz  - WorldSpacePosition );
    
				float2 uv_Normals = IN.ase_texcoord7.xy * _Normals_ST.xy + _Normals_ST.zw;
				float3 tex2DNode560 = UnpackNormalmapRGorAG( tex2D( _Normals, uv_Normals ), 1.0f );
				float3 tangentNormal561 = tex2DNode560;
				
				#if defined(_DIRECTION_X)
				float staticSwitch495 = IN.ase_texcoord8.xyz.x;
				#elif defined(_DIRECTION_Y)
				float staticSwitch495 = IN.ase_texcoord8.xyz.y;
				#elif defined(_DIRECTION_Z)
				float staticSwitch495 = IN.ase_texcoord8.xyz.z;
				#else
				float staticSwitch495 = IN.ase_texcoord8.xyz.y;
				#endif
				#ifdef _INVERTDIRECTION_ON
				float staticSwitch494 = -staticSwitch495;
				#else
				float staticSwitch494 = staticSwitch495;
				#endif
				float temp_output_499_0 = ( _SizeSpeedInterval.x * _SizeSpeedInterval.y );
				float mulTime440 = _Time.y * temp_output_499_0;
				float temp_output_463_0 = ( fmod( ( ( staticSwitch494 * _SizeSpeedInterval.x ) + mulTime440 ) , ( ( temp_output_499_0 + 1.0 ) * _SizeSpeedInterval.z ) ) + ( _TailHeadFalloff - 1.0 ) );
				float3 tanToWorld0 = float3( WorldSpaceTangent.x, WorldSpaceBiTangent.x, WorldSpaceNormal.x );
				float3 tanToWorld1 = float3( WorldSpaceTangent.y, WorldSpaceBiTangent.y, WorldSpaceNormal.y );
				float3 tanToWorld2 = float3( WorldSpaceTangent.z, WorldSpaceBiTangent.z, WorldSpaceNormal.z );
				float3 tanNormal568 = tex2DNode560;
				float3 worldNormal568 = float3(dot(tanToWorld0,tanNormal568), dot(tanToWorld1,tanNormal568), dot(tanToWorld2,tanNormal568));
				float3 worldNormal567 = worldNormal568;
				float fresnelNdotV400 = dot( worldNormal567, WorldSpaceViewDirection );
				float fresnelNode400 = ( _GlintFresnel.x + _GlintFresnel.y * pow( 1.0 - fresnelNdotV400, _GlintFresnel.z ) );
				float4 screenPos = IN.ase_texcoord9;
				float4 ase_screenPosNorm = screenPos / screenPos.w;
				ase_screenPosNorm.z = ( UNITY_NEAR_CLIP_VALUE >= 0 ) ? ase_screenPosNorm.z : ase_screenPosNorm.z * 0.5 + 0.5;
				float2 uv_Noise = IN.ase_texcoord7.xy * _Noise_ST.xy + _Noise_ST.zw;
				float2 temp_output_354_0 = ( uv_Noise * _Frequency );
				float mulTime360 = _Time.y * _SpakleSpeed;
				float smoothstepResult364 = smoothstep( _Threshold , ( _Threshold + _Range ) , ( tex2D( _Noise, ( ( (ase_screenPosNorm).xy * _ScreenContribution ) + temp_output_354_0 + mulTime360 ) ).g * tex2D( _Noise, ( ( temp_output_354_0 * 1.1 ) + -mulTime360 ) ).g ));
				float fresnelNdotV536 = dot( worldNormal567, WorldSpaceViewDirection );
				float fresnelNode536 = ( _SparkleFresnel.x + _SparkleFresnel.y * pow( 1.0 - fresnelNdotV536, _SparkleFresnel.z ) );
				float fresnelNdotV370 = dot( worldNormal567, WorldSpaceViewDirection );
				float fresnelNode370 = ( _MainGlowFresnel.x + _MainGlowFresnel.y * pow( 1.0 - fresnelNdotV370, _MainGlowFresnel.z ) );
				
				
		        float3 Albedo = _Albedo.rgb;
				float3 Normal = tangentNormal561;
				float3 Emission = ( ( _GlintColor * pow( ( 1.0 - ( saturate( ( temp_output_463_0 * ( -1.0 / ( 1.0 - _TailHeadFalloff ) ) ) ) + saturate( ( temp_output_463_0 * ( 1.0 / _TailHeadFalloff ) ) ) ) ) , 5.0 ) * fresnelNode400 * _Brightness ) + ( _SparkleColor * smoothstepResult364 * _SparklesBrightness * fresnelNode536 ) + ( _BodyGlow * fresnelNode370 ) ).rgb;
				float3 Specular = float3(0.5, 0.5, 0.5);
				float Metallic = 0.9;
				float Smoothness = 0.9;
				float Occlusion = 1;
				float Alpha = 1;
				float AlphaClipThreshold = 0;

        		InputData inputData;
        		inputData.positionWS = WorldSpacePosition;

        #ifdef _NORMALMAP
        	    inputData.normalWS = normalize(TransformTangentToWorld(Normal, half3x3(WorldSpaceTangent, WorldSpaceBiTangent, WorldSpaceNormal)));
        #else
            #if !SHADER_HINT_NICE_QUALITY
                inputData.normalWS = WorldSpaceNormal;
            #else
        	    inputData.normalWS = normalize(WorldSpaceNormal);
            #endif
        #endif

        #if !SHADER_HINT_NICE_QUALITY
        	    // viewDirection should be normalized here, but we avoid doing it as it's close enough and we save some ALU.
        	    inputData.viewDirectionWS = WorldSpaceViewDirection;
        #else
        	    inputData.viewDirectionWS = normalize(WorldSpaceViewDirection);
        #endif

        	    inputData.shadowCoord = IN.shadowCoord;

        	    inputData.fogCoord = IN.fogFactorAndVertexLight.x;
        	    inputData.vertexLighting = IN.fogFactorAndVertexLight.yzw;
        	    inputData.bakedGI = SAMPLE_GI(IN.lightmapUVOrVertexSH.xy, IN.lightmapUVOrVertexSH.xyz, inputData.normalWS);

        		half4 color = LightweightFragmentPBR(
        			inputData, 
        			Albedo, 
        			Metallic, 
        			Specular, 
        			Smoothness, 
        			Occlusion, 
        			Emission, 
        			Alpha);

			#ifdef TERRAIN_SPLAT_ADDPASS
				color.rgb = MixFogColor(color.rgb, half3( 0, 0, 0 ), IN.fogFactorAndVertexLight.x );
			#else
				color.rgb = MixFog(color.rgb, IN.fogFactorAndVertexLight.x);
			#endif

        #if _AlphaClip
        		clip(Alpha - AlphaClipThreshold);
        #endif

		#if ASE_LW_FINAL_COLOR_ALPHA_MULTIPLY
				color.rgb *= color.a;
		#endif
        		return color;
            }

        	ENDHLSL
        }

		
        Pass
        {
			
        	Name "ShadowCaster"
            Tags { "LightMode"="ShadowCaster" }

			ZWrite On
			ZTest LEqual

            HLSLPROGRAM
            // Required to compile gles 2.0 with standard srp library
            #pragma prefer_hlslcc gles
            #pragma exclude_renderers d3d11_9x
            

            //--------------------------------------
            // GPU Instancing
            #pragma multi_compile_instancing

            #pragma vertex ShadowPassVertex
            #pragma fragment ShadowPassFragment

            

            #include "Packages/com.unity.render-pipelines.lightweight/ShaderLibrary/Core.hlsl"
            #include "Packages/com.unity.render-pipelines.lightweight/ShaderLibrary/Lighting.hlsl"
            #include "Packages/com.unity.render-pipelines.lightweight/ShaderLibrary/ShaderGraphFunctions.hlsl"
            #include "Packages/com.unity.render-pipelines.core/ShaderLibrary/Color.hlsl"

            CBUFFER_START(UnityPerMaterial)
						CBUFFER_END
			
			
            struct GraphVertexInput
            {
                float4 vertex : POSITION;
                float3 ase_normal : NORMAL;
				
                UNITY_VERTEX_INPUT_INSTANCE_ID
            };


        	struct VertexOutput
        	{
        	    float4 clipPos      : SV_POSITION;
                
                UNITY_VERTEX_INPUT_INSTANCE_ID
        	};

            // x: global clip space bias, y: normal world space bias
            float4 _ShadowBias;
            float3 _LightDirection;

            VertexOutput ShadowPassVertex(GraphVertexInput v)
        	{
        	    VertexOutput o;
        	    UNITY_SETUP_INSTANCE_ID(v);
                UNITY_TRANSFER_INSTANCE_ID(v, o);

				
				float3 vertexValue =  float3(0,0,0) ;
				#ifdef ASE_ABSOLUTE_VERTEX_POS
				v.vertex.xyz = vertexValue;
				#else
				v.vertex.xyz += vertexValue;
				#endif

				v.ase_normal =  v.ase_normal ;

        	    float3 positionWS = TransformObjectToWorld(v.vertex.xyz);
                float3 normalWS = TransformObjectToWorldDir(v.ase_normal);

                float invNdotL = 1.0 - saturate(dot(_LightDirection, normalWS));
                float scale = invNdotL * _ShadowBias.y;

                // normal bias is negative since we want to apply an inset normal offset
                positionWS = normalWS * scale.xxx + positionWS;
                float4 clipPos = TransformWorldToHClip(positionWS);

                // _ShadowBias.x sign depens on if platform has reversed z buffer
                clipPos.z += _ShadowBias.x;

        	#if UNITY_REVERSED_Z
        	    clipPos.z = min(clipPos.z, clipPos.w * UNITY_NEAR_CLIP_VALUE);
        	#else
        	    clipPos.z = max(clipPos.z, clipPos.w * UNITY_NEAR_CLIP_VALUE);
        	#endif
                o.clipPos = clipPos;

        	    return o;
        	}

            half4 ShadowPassFragment(VertexOutput IN) : SV_TARGET
            {
                UNITY_SETUP_INSTANCE_ID(IN);

               

				float Alpha = 1;
				float AlphaClipThreshold = AlphaClipThreshold;

         #if _AlphaClip
        		clip(Alpha - AlphaClipThreshold);
        #endif
                return 0;
            }

            ENDHLSL
        }

		
        Pass
        {
			
        	Name "DepthOnly"
            Tags { "LightMode"="DepthOnly" }

            ZWrite On
			ColorMask 0

            HLSLPROGRAM
            // Required to compile gles 2.0 with standard srp library
            #pragma prefer_hlslcc gles
            #pragma exclude_renderers d3d11_9x

            //--------------------------------------
            // GPU Instancing
            #pragma multi_compile_instancing

            #pragma vertex vert
            #pragma fragment frag

            

            #include "Packages/com.unity.render-pipelines.lightweight/ShaderLibrary/Core.hlsl"
            #include "Packages/com.unity.render-pipelines.lightweight/ShaderLibrary/Lighting.hlsl"
            #include "Packages/com.unity.render-pipelines.lightweight/ShaderLibrary/ShaderGraphFunctions.hlsl"
            #include "Packages/com.unity.render-pipelines.core/ShaderLibrary/Color.hlsl"

			CBUFFER_START(UnityPerMaterial)
						CBUFFER_END
			
			
           
            struct GraphVertexInput
            {
                float4 vertex : POSITION;
				float3 ase_normal : NORMAL;
				
                UNITY_VERTEX_INPUT_INSTANCE_ID
            };


        	struct VertexOutput
        	{
        	    float4 clipPos      : SV_POSITION;
                
                UNITY_VERTEX_INPUT_INSTANCE_ID
                UNITY_VERTEX_OUTPUT_STEREO
        	};

            VertexOutput vert(GraphVertexInput v)
            {
                VertexOutput o = (VertexOutput)0;
        	    UNITY_SETUP_INSTANCE_ID(v);
                UNITY_TRANSFER_INSTANCE_ID(v, o);
                UNITY_INITIALIZE_VERTEX_OUTPUT_STEREO(o);

				
				float3 vertexValue =  float3(0,0,0) ;
				#ifdef ASE_ABSOLUTE_VERTEX_POS
				v.vertex.xyz = vertexValue;
				#else
				v.vertex.xyz += vertexValue;
				#endif

				v.ase_normal =  v.ase_normal ;

        	    o.clipPos = TransformObjectToHClip(v.vertex.xyz);
        	    return o;
            }

            half4 frag(VertexOutput IN) : SV_TARGET
            {
                UNITY_SETUP_INSTANCE_ID(IN);

				

				float Alpha = 1;
				float AlphaClipThreshold = AlphaClipThreshold;

         #if _AlphaClip
        		clip(Alpha - AlphaClipThreshold);
        #endif
                return 0;
            }
            ENDHLSL
        }

        // This pass it not used during regular rendering, only for lightmap baking.
		
        Pass
        {
			
        	Name "Meta"
            Tags { "LightMode"="Meta" }

            Cull Off

            HLSLPROGRAM
            // Required to compile gles 2.0 with standard srp library
            #pragma prefer_hlslcc gles
            #pragma exclude_renderers d3d11_9x
            

            #pragma vertex vert
            #pragma fragment frag


            

			uniform float4 _MainTex_ST;

            #include "Packages/com.unity.render-pipelines.lightweight/ShaderLibrary/Core.hlsl"
            #include "Packages/com.unity.render-pipelines.lightweight/ShaderLibrary/MetaInput.hlsl"
            #include "Packages/com.unity.render-pipelines.lightweight/ShaderLibrary/ShaderGraphFunctions.hlsl"
            #include "Packages/com.unity.render-pipelines.core/ShaderLibrary/Color.hlsl"

			CBUFFER_START(UnityPerMaterial)
						CBUFFER_END
			
			
            #pragma shader_feature _ _SMOOTHNESS_TEXTURE_ALBEDO_CHANNEL_A
            #pragma shader_feature EDITOR_VISUALIZATION


            struct GraphVertexInput
            {
                float4 vertex : POSITION;
				float3 ase_normal : NORMAL;
				float4 texcoord1 : TEXCOORD1;
				
                UNITY_VERTEX_INPUT_INSTANCE_ID
            };

        	struct VertexOutput
        	{
        	    float4 clipPos      : SV_POSITION;
                
                UNITY_VERTEX_INPUT_INSTANCE_ID
                UNITY_VERTEX_OUTPUT_STEREO
        	};

            VertexOutput vert(GraphVertexInput v)
            {
                VertexOutput o = (VertexOutput)0;
        	    UNITY_SETUP_INSTANCE_ID(v);
                UNITY_TRANSFER_INSTANCE_ID(v, o);
                UNITY_INITIALIZE_VERTEX_OUTPUT_STEREO(o);
				

				float3 vertexValue =  float3(0,0,0) ;
				#ifdef ASE_ABSOLUTE_VERTEX_POS
				v.vertex.xyz = vertexValue;
				#else
				v.vertex.xyz += vertexValue;
				#endif

				v.ase_normal =  v.ase_normal ;
				
                o.clipPos = MetaVertexPosition(v.vertex, v.texcoord1.xy, v.texcoord1.xy, unity_LightmapST);
        	    return o;
            }

            half4 frag(VertexOutput IN) : SV_TARGET
            {
                UNITY_SETUP_INSTANCE_ID(IN);

           		
				
		        float3 Albedo = float3(0.5, 0.5, 0.5);
				float3 Emission = 0;
				float Alpha = 1;
				float AlphaClipThreshold = 0;

         #if _AlphaClip
        		clip(Alpha - AlphaClipThreshold);
        #endif

                MetaInput metaInput = (MetaInput)0;
                metaInput.Albedo = Albedo;
                metaInput.Emission = Emission;
                
                return MetaFragment(metaInput);
            }
            ENDHLSL
        }
		
    }
    FallBack "Hidden/InternalErrorShader"
	CustomEditor "ASEMaterialInspector"
	
	
}
/*ASEBEGIN
Version=16201
442;73;933;593;-1322.161;2085.049;5.863735;True;False
Node;AmplifyShaderEditor.CommentaryNode;569;1344.167,-453.8903;Float;False;842.9095;339.4828;Normals;4;560;568;567;561;;0.2573529,0.3546652,1,1;0;0
Node;AmplifyShaderEditor.CommentaryNode;570;2400,-656;Float;False;2555;885;;28;510;355;354;555;554;366;360;511;359;365;508;507;515;524;557;525;556;363;364;367;369;346;347;536;368;564;571;419;Sparkles Effect;1,1,1,1;0;0
Node;AmplifyShaderEditor.CommentaryNode;429;1352.417,-1965.659;Float;False;3595.187;815.0569;;31;488;495;492;505;499;494;440;497;447;466;491;439;504;489;442;478;470;463;468;477;484;483;473;424;486;487;401;431;400;390;562;Glint Effect;0.9986145,1,0.4103774,1;0;0
Node;AmplifyShaderEditor.StaticSwitch;494;2064,-1840;Float;False;Property;_InvertDirection;Invert Direction;4;0;Create;True;0;0;False;0;0;1;1;True;;Toggle;2;X;Y;9;1;FLOAT;0;False;0;FLOAT;0;False;2;FLOAT;0;False;3;FLOAT;0;False;4;FLOAT;0;False;5;FLOAT;0;False;6;FLOAT;0;False;7;FLOAT;0;False;8;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleTimeNode;440;2112,-1680;Float;False;1;0;FLOAT;0.5;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleAddOpNode;447;2112,-1600;Float;False;2;2;0;FLOAT;0;False;1;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;499;1920,-1632;Float;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.StaticSwitch;495;1696,-1840;Float;False;Property;_Direction;Direction;3;0;Create;True;0;0;False;0;0;1;1;True;;KeywordEnum;3;X;Y;Z;9;1;FLOAT;0;False;0;FLOAT;0;False;2;FLOAT;0;False;3;FLOAT;0;False;4;FLOAT;0;False;5;FLOAT;0;False;6;FLOAT;0;False;7;FLOAT;0;False;8;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.PosVertexDataNode;488;1440,-1856;Float;True;0;0;5;FLOAT3;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.Vector3Node;505;1440,-1664;Float;False;Property;_SizeSpeedInterval;Size Speed Interval;5;0;Create;True;0;0;False;0;1,1,1;0.5,10,5;0;4;FLOAT3;0;FLOAT;1;FLOAT;2;FLOAT;3
Node;AmplifyShaderEditor.NegateNode;492;1936,-1776;Float;False;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.TextureCoordinatesNode;510;2720,-336;Float;False;0;-1;2;3;2;SAMPLER2D;;False;0;FLOAT2;1,1;False;1;FLOAT2;0,0;False;5;FLOAT2;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.RangedFloatNode;511;2656,-32;Float;False;Property;_SpakleSpeed;Spakle Speed;15;0;Create;True;0;0;False;0;0;0.002;0;0.1;0;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleSubtractOpNode;504;3008,-1696;Float;False;2;0;FLOAT;0;False;1;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;355;2704,-176;Float;False;Property;_Frequency;Frequency;11;0;Create;True;0;0;False;0;20;4;0;100;0;1;FLOAT;0
Node;AmplifyShaderEditor.ScreenPosInputsNode;524;2464,-608;Float;False;0;False;0;5;FLOAT4;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.TexturePropertyNode;515;2448,-416;Float;True;Property;_Noise;Noise;10;1;[NoScaleOffset];Create;True;0;0;False;0;None;bdbe94d7623ec3940947b62544306f1c;False;white;Auto;Texture2D;0;1;SAMPLER2D;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;497;2352,-1792;Float;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;556;3024,-544;Float;False;2;2;0;FLOAT2;0,0;False;1;FLOAT;0;False;1;FLOAT2;0
Node;AmplifyShaderEditor.SimpleDivideOpNode;470;3200,-1472;Float;False;2;0;FLOAT;1;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;491;2272,-1600;Float;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleTimeNode;360;2960,-32;Float;False;1;0;FLOAT;0.1;False;1;FLOAT;0
Node;AmplifyShaderEditor.ComponentMaskNode;525;2768,-608;Float;False;True;True;False;False;1;0;FLOAT4;0,0,0,0;False;1;FLOAT2;0
Node;AmplifyShaderEditor.SimpleDivideOpNode;478;3200,-1600;Float;False;2;0;FLOAT;-1;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleAddOpNode;463;3216,-1760;Float;False;2;2;0;FLOAT;0;False;1;FLOAT;-0.5;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;557;2672,-480;Float;False;Property;_ScreenContribution;Screen Contribution;16;0;Create;True;0;0;False;0;0.2;0.2;0;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.OneMinusNode;489;3008,-1568;Float;False;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.FmodOpNode;442;2752,-1760;Float;False;2;0;FLOAT;0;False;1;FLOAT;3;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;555;3024,-160;Float;False;Constant;_Float4;Float 4;19;0;Create;True;0;0;False;0;1.1;0;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;354;3024,-256;Float;False;2;2;0;FLOAT2;0,0;False;1;FLOAT;0;False;1;FLOAT2;0
Node;AmplifyShaderEditor.SimpleAddOpNode;359;3440,-272;Float;False;3;3;0;FLOAT2;0,0;False;1;FLOAT2;0,0;False;2;FLOAT;0;False;1;FLOAT2;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;477;3504,-1760;Float;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SamplerNode;560;1394.167,-403.8903;Float;True;Property;_Normals;Normals;1;0;Create;True;0;0;False;0;None;None;True;0;True;bump;Auto;True;Object;-1;Auto;Texture2D;6;0;SAMPLER2D;;False;1;FLOAT2;0,0;False;2;FLOAT;0;False;3;FLOAT2;0,0;False;4;FLOAT2;0,0;False;5;FLOAT;1;False;5;FLOAT3;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.SimpleAddOpNode;365;3440,-128;Float;False;2;2;0;FLOAT2;0,0;False;1;FLOAT;0;False;1;FLOAT2;0
Node;AmplifyShaderEditor.NegateNode;366;3232,-32;Float;False;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;554;3216,-192;Float;False;2;2;0;FLOAT2;0,0;False;1;FLOAT;0;False;1;FLOAT2;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;468;3504,-1536;Float;False;2;2;0;FLOAT;0;False;1;FLOAT;2.25;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;466;2656,-1456;Float;False;Property;_TailHeadFalloff;Tail Head Falloff;7;0;Create;True;0;0;False;0;0.5;0.9;0;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleAddOpNode;439;2528,-1760;Float;False;2;2;0;FLOAT;0;False;1;FLOAT;-0.27;False;1;FLOAT;0
Node;AmplifyShaderEditor.SamplerNode;507;3632,-368;Float;True;Property;_TextureSample0;Texture Sample 0;14;0;Create;True;0;0;False;0;None;e28dc97a9541e3642a48c0e3886688c5;True;0;False;white;Auto;False;Object;-1;Auto;Texture2D;6;0;SAMPLER2D;;False;1;FLOAT2;0,0;False;2;FLOAT;0;False;3;FLOAT2;0,0;False;4;FLOAT2;0,0;False;5;FLOAT;1;False;5;COLOR;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.RangedFloatNode;367;3648,32;Float;False;Property;_Threshold;Threshold;12;0;Create;True;0;0;False;0;0.5;0.5;0;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleAddOpNode;473;3840,-1664;Float;False;2;2;0;FLOAT;0;False;1;FLOAT;-1;False;1;FLOAT;0
Node;AmplifyShaderEditor.WorldNormalVector;568;1732.077,-293.4075;Float;False;False;1;0;FLOAT3;0,0,0;False;4;FLOAT3;0;FLOAT;1;FLOAT;2;FLOAT;3
Node;AmplifyShaderEditor.SamplerNode;508;3632,-160;Float;True;Property;_TextureSample1;Texture Sample 1;13;0;Create;True;0;0;False;0;None;e28dc97a9541e3642a48c0e3886688c5;True;0;False;white;Auto;False;Object;-1;Auto;Texture2D;6;0;SAMPLER2D;;False;1;FLOAT2;0,0;False;2;FLOAT;0;False;3;FLOAT2;0,0;False;4;FLOAT2;0,0;False;5;FLOAT;1;False;5;COLOR;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.SaturateNode;483;3648,-1760;Float;False;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SaturateNode;484;3648,-1536;Float;False;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;369;3648,112;Float;False;Property;_Range;Range;13;0;Create;True;0;0;False;0;0;0.5;0;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.OneMinusNode;486;3984,-1664;Float;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;564;4218.197,-93.65095;Float;False;567;worldNormal;1;0;OBJECT;0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;567;1944.077,-295.4075;Float;False;worldNormal;-1;True;1;0;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.Vector3Node;572;4264.248,588.1351;Float;False;Property;_MainGlowFresnel;Fresnel Bias, Scale, Power;19;0;Create;False;0;0;False;0;0,0,0;0.02,1,5;0;4;FLOAT3;0;FLOAT;1;FLOAT;2;FLOAT;3
Node;AmplifyShaderEditor.GetLocalVarNode;563;4328.248,508.1352;Float;False;567;worldNormal;1;0;OBJECT;0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;363;3984,-240;Float;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;562;3808,-1424;Float;False;567;worldNormal;1;0;OBJECT;0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.Vector3Node;424;3808,-1344;Float;False;Property;_GlintFresnel;Fresnel Bias, Scale, Power;6;0;Create;False;0;0;False;0;0,0,0;0.01,3,4;0;4;FLOAT3;0;FLOAT;1;FLOAT;2;FLOAT;3
Node;AmplifyShaderEditor.SmoothstepOpNode;364;4224,-240;Float;False;3;0;FLOAT;0;False;1;FLOAT;0.8;False;2;FLOAT;0.85;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleAddOpNode;368;4048,0;Float;False;2;2;0;FLOAT;0;False;1;FLOAT;0.05;False;1;FLOAT;0
Node;AmplifyShaderEditor.Vector3Node;571;4208,0;Float;False;Property;_SparkleFresnel;Fresnel Bias, Scale, Power;17;0;Create;False;0;0;False;0;0,0,0;0.02,0.8,2;0;4;FLOAT3;0;FLOAT;1;FLOAT;2;FLOAT;3
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;390;4765.775,-1686.948;Float;False;4;4;0;COLOR;0,0,0,0;False;1;FLOAT;0;False;2;FLOAT;0;False;3;FLOAT;0;False;1;COLOR;0
Node;AmplifyShaderEditor.ColorNode;550;4672,382.1486;Float;False;Property;_BodyGlow;Color;18;0;Create;False;0;0;False;1;Header(Body Glow);0,0,0,0;0.6323529,0.4794762,0,0;False;0;5;COLOR;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.ColorNode;347;4528,-384;Float;False;Property;_SparkleColor;Color;9;0;Create;False;0;0;False;2;Space(10);Header(Sparkles);0,0,0,0;1,0.7779999,0.2971669,1;False;0;5;COLOR;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.FresnelNode;370;4584.248,572.1351;Float;True;Standard;WorldNormal;ViewDir;False;5;0;FLOAT3;0,0,0;False;4;FLOAT3;0,0,0;False;1;FLOAT;0.02;False;2;FLOAT;1;False;3;FLOAT;5;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;431;4480,-1360;Float;False;Property;_Brightness;Brightness;8;0;Create;True;0;0;False;0;1;5;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.ColorNode;401;4432,-1824;Float;False;Property;_GlintColor;Color;2;0;Create;False;0;0;False;1;Header(Glint Effect);0,0,0,0;1,0.9154145,0.4292426,0;False;0;5;COLOR;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.RangedFloatNode;419;4576,-160;Float;False;Property;_SparklesBrightness;Brightness;14;0;Create;False;0;0;False;0;2;5;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.PowerNode;487;4176,-1664;Float;True;2;0;FLOAT;0;False;1;FLOAT;5;False;1;FLOAT;0
Node;AmplifyShaderEditor.FresnelNode;400;4187.048,-1422.668;Float;True;Standard;WorldNormal;ViewDir;False;5;0;FLOAT3;0,0,0;False;4;FLOAT3;0,0,0;False;1;FLOAT;0.01;False;2;FLOAT;1;False;3;FLOAT;4;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;345;5257.424,-157.0255;Float;False;Constant;_Float12;Float 12;20;0;Create;True;0;0;False;0;0.9;0;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleAddOpNode;374;5200,-288;Float;False;3;3;0;COLOR;0,0,0,0;False;1;COLOR;0,0,0,0;False;2;COLOR;0,0,0,0;False;1;COLOR;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;561;1718.788,-401.7002;Float;False;tangentNormal;-1;True;1;0;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.GetLocalVarNode;565;5203.372,-430.7056;Float;False;561;tangentNormal;1;0;OBJECT;0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.ColorNode;292;5204.548,-643.8364;Float;False;Property;_Albedo;Albedo;0;0;Create;True;0;0;False;0;0,0,0,0;0.7647059,0.5730754,0.1124562,0;False;0;5;COLOR;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;371;4992,384;Float;False;2;2;0;COLOR;0,0,0,0;False;1;FLOAT;0;False;1;COLOR;0
Node;AmplifyShaderEditor.FresnelNode;536;4528,-48;Float;True;Standard;WorldNormal;ViewDir;False;5;0;FLOAT3;0,0,0;False;4;FLOAT3;0,0,0;False;1;FLOAT;0.01;False;2;FLOAT;0.8;False;3;FLOAT;2;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;346;4816,-256;Float;False;4;4;0;COLOR;0,0,0,0;False;1;FLOAT;0;False;2;FLOAT;0;False;3;FLOAT;0;False;1;COLOR;0
Node;AmplifyShaderEditor.TemplateMultiPassMasterNode;574;5576.469,-391.7877;Float;False;False;2;Float;ASEMaterialInspector;0;1;ASETemplateShaders/LightWeight;1976390536c6c564abb90fe41f6ee334;0;1;ShadowCaster;0;False;False;False;True;0;False;-1;False;False;False;False;False;True;3;RenderPipeline=LightweightPipeline;RenderType=Opaque=RenderType;Queue=Geometry=Queue=0;True;2;0;False;False;False;False;False;False;True;1;False;-1;True;3;False;-1;False;True;1;LightMode=ShadowCaster;False;0;;0;0;Standard;0;4;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT3;0,0,0;False;3;FLOAT3;0,0,0;False;0
Node;AmplifyShaderEditor.TemplateMultiPassMasterNode;575;5576.469,-391.7877;Float;False;False;2;Float;ASEMaterialInspector;0;1;ASETemplateShaders/LightWeight;1976390536c6c564abb90fe41f6ee334;0;2;DepthOnly;0;False;False;False;True;0;False;-1;False;False;False;False;False;True;3;RenderPipeline=LightweightPipeline;RenderType=Opaque=RenderType;Queue=Geometry=Queue=0;True;2;0;False;False;False;False;True;False;False;False;False;0;False;-1;False;True;1;False;-1;False;False;True;1;LightMode=DepthOnly;False;0;;0;0;Standard;0;4;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT3;0,0,0;False;3;FLOAT3;0,0,0;False;0
Node;AmplifyShaderEditor.TemplateMultiPassMasterNode;573;5576.469,-401.7877;Float;False;True;2;Float;ASEMaterialInspector;0;2;ASESampleShaders/SRP Lightweight/GlintSparkle;1976390536c6c564abb90fe41f6ee334;0;0;Base;11;False;False;False;True;0;False;-1;False;False;False;False;False;True;3;RenderPipeline=LightweightPipeline;RenderType=Opaque=RenderType;Queue=Geometry=Queue=0;True;2;0;True;1;1;False;-1;0;False;-1;0;1;False;-1;0;False;-1;False;False;False;True;True;True;True;True;0;False;-1;True;False;255;False;-1;255;False;-1;255;False;-1;7;False;-1;1;False;-1;1;False;-1;1;False;-1;7;False;-1;1;False;-1;1;False;-1;1;False;-1;True;1;False;-1;True;3;False;-1;True;True;0;False;-1;0;False;-1;True;1;LightMode=LightweightForward;False;0;;0;0;Standard;1;Vertex Position,InvertActionOnDeselection;1;1;_FinalColorxAlpha;0;4;True;True;True;True;11;0;FLOAT3;0,0,0;False;1;FLOAT3;0,0,0;False;2;FLOAT3;0,0,0;False;9;FLOAT3;0,0,0;False;3;FLOAT;0;False;4;FLOAT;0;False;5;FLOAT;0;False;6;FLOAT;0;False;7;FLOAT;0;False;8;FLOAT3;0,0,0;False;10;FLOAT3;0,0,0;False;0
Node;AmplifyShaderEditor.TemplateMultiPassMasterNode;576;5576.469,-371.7877;Float;False;False;2;Float;ASEMaterialInspector;0;1;Hidden/Templates/LightWeightSRPPBR;1976390536c6c564abb90fe41f6ee334;0;3;Meta;0;False;False;False;True;0;False;-1;False;False;False;False;False;True;3;RenderPipeline=LightweightPipeline;RenderType=Opaque=RenderType;Queue=Geometry=Queue=0;True;2;0;False;False;False;True;2;False;-1;False;False;False;False;False;True;1;LightMode=Meta;False;0;;0;0;Standard;0;6;0;FLOAT3;0,0,0;False;1;FLOAT3;0,0,0;False;2;FLOAT;0;False;3;FLOAT;0;False;4;FLOAT3;0,0,0;False;5;FLOAT3;0,0,0;False;0
WireConnection;494;1;495;0
WireConnection;494;0;492;0
WireConnection;440;0;499;0
WireConnection;447;0;499;0
WireConnection;499;0;505;1
WireConnection;499;1;505;2
WireConnection;495;1;488;1
WireConnection;495;0;488;2
WireConnection;495;2;488;3
WireConnection;492;0;495;0
WireConnection;510;2;515;0
WireConnection;504;0;466;0
WireConnection;497;0;494;0
WireConnection;497;1;505;1
WireConnection;556;0;525;0
WireConnection;556;1;557;0
WireConnection;470;1;466;0
WireConnection;491;0;447;0
WireConnection;491;1;505;3
WireConnection;360;0;511;0
WireConnection;525;0;524;0
WireConnection;478;1;489;0
WireConnection;463;0;442;0
WireConnection;463;1;504;0
WireConnection;489;0;466;0
WireConnection;442;0;439;0
WireConnection;442;1;491;0
WireConnection;354;0;510;0
WireConnection;354;1;355;0
WireConnection;359;0;556;0
WireConnection;359;1;354;0
WireConnection;359;2;360;0
WireConnection;477;0;463;0
WireConnection;477;1;478;0
WireConnection;365;0;554;0
WireConnection;365;1;366;0
WireConnection;366;0;360;0
WireConnection;554;0;354;0
WireConnection;554;1;555;0
WireConnection;468;0;463;0
WireConnection;468;1;470;0
WireConnection;439;0;497;0
WireConnection;439;1;440;0
WireConnection;507;0;515;0
WireConnection;507;1;359;0
WireConnection;473;0;483;0
WireConnection;473;1;484;0
WireConnection;568;0;560;0
WireConnection;508;0;515;0
WireConnection;508;1;365;0
WireConnection;483;0;477;0
WireConnection;484;0;468;0
WireConnection;486;0;473;0
WireConnection;567;0;568;0
WireConnection;363;0;507;2
WireConnection;363;1;508;2
WireConnection;364;0;363;0
WireConnection;364;1;367;0
WireConnection;364;2;368;0
WireConnection;368;0;367;0
WireConnection;368;1;369;0
WireConnection;390;0;401;0
WireConnection;390;1;487;0
WireConnection;390;2;400;0
WireConnection;390;3;431;0
WireConnection;370;0;563;0
WireConnection;370;1;572;1
WireConnection;370;2;572;2
WireConnection;370;3;572;3
WireConnection;487;0;486;0
WireConnection;400;0;562;0
WireConnection;400;1;424;1
WireConnection;400;2;424;2
WireConnection;400;3;424;3
WireConnection;374;0;390;0
WireConnection;374;1;346;0
WireConnection;374;2;371;0
WireConnection;561;0;560;0
WireConnection;371;0;550;0
WireConnection;371;1;370;0
WireConnection;536;0;564;0
WireConnection;536;1;571;1
WireConnection;536;2;571;2
WireConnection;536;3;571;3
WireConnection;346;0;347;0
WireConnection;346;1;364;0
WireConnection;346;2;419;0
WireConnection;346;3;536;0
WireConnection;573;0;292;0
WireConnection;573;1;565;0
WireConnection;573;2;374;0
WireConnection;573;3;345;0
WireConnection;573;4;345;0
ASEEND*/
//CHKSM=C4B6D5BDF4EDE105985C8733848207165C501598
