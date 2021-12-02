{"name":"ReadFile","args":{"hFile":1724,"lpBuffer":1695984,"nNumberOfBytesToRead":8,"lpNumberOfBytesRead":1695832,"lpOverlapped":0}} 
14 1695984 0x19e0f0 8 [142,69,229,31,235,151,101,252] 
0x7384E9 jne short 0002BA1Eh 
0x7384F8 mov eax,[esp+18h] - EAX=1- mem[1695832]=Ok(8) 
0x7384FC add [esi+44h],eax - mem[181321500]=Ok(96)- EAX=8 
0x7384FF pop edi - EDI=1695984 
0x738500 pop ebp - EBP=1975661760 
0x738501 pop ebx - EBX=8 
0x738502 pop esi - ESI=181321432 
0x738503 ret 8 

0x44A51A pop esi - ESI=1696168 
0x44A51B pop ebp - EBP=8 
0x44A51C ret 8 

0x49694C jmp short 00017C3Eh 
0x49696B mov dword [esp+24h],0Fh - mem[1695892]=Ok(16) 
0x496973 mov dword [esp+20h],0 - mem[1695888]=Ok(5639113) 
0x49697B mov byte [esp+10h],0 - mem[1695872]=Ok(1695904) 
0x496980 test dword [8BF7FCh],20000000h - mem[9172988]=Ok(536870912) 
0x49698A mov dword [esp+30h],0 - mem[1695904]=Ok(4294967295) 
0x496992 jne short 00017C8Dh 
0x4969BA push 14h 
0x4969BC mov ecx,8BF7ECh - ECX=4249007335 
0x4969C1 call 0FFFB60E3h 

0x434E10 mov eax,[ecx+20h] - EAX=8- mem[9173004]=Ok(9173440) 
0x434E13 test eax,eax - EAX=9173440- EAX=9173440 
0x434E15 je short 00005C12h 
0x434E17 mov ecx,[esp+4] - ECX=9172972- mem[1695852]=Ok(20) 
0x434E1B jmp short 00005C07h 
0x434E20 cmp [eax],ecx - mem[9173440]=Ok(10)- ECX=20 
0x434E22 je short 00005C17h 
0x434E24 mov eax,[eax+8] - EAX=9173440- mem[9173448]=Ok(9173576) 
0x434E27 test eax,eax - EAX=9173576- EAX=9173576 
0x434E29 jne short 00005C07h 
0x434E20 cmp [eax],ecx - mem[9173576]=Ok(23)- ECX=20 
0x434E22 je short 00005C17h 
0x434E24 mov eax,[eax+8] - EAX=9173576- mem[9173584]=Ok(9172464) 
0x434E27 test eax,eax - EAX=9172464- EAX=9172464 
0x434E29 jne short 00005C07h 
0x434E20 cmp [eax],ecx - mem[9172464]=Ok(15)- ECX=20 
0x434E22 je short 00005C17h 
0x434E24 mov eax,[eax+8] - EAX=9172464- mem[9172472]=Ok(9172476) 
0x434E27 test eax,eax - EAX=9172476- EAX=9172476 
0x434E29 jne short 00005C07h 
0x434E20 cmp [eax],ecx - mem[9172476]=Ok(6)- ECX=20 
0x434E22 je short 00005C17h 
0x434E24 mov eax,[eax+8] - EAX=9172476- mem[9172484]=Ok(9172488) 
0x434E27 test eax,eax - EAX=9172488- EAX=9172488 
0x434E29 jne short 00005C07h 
0x434E20 cmp [eax],ecx - mem[9172488]=Ok(20)- ECX=20 
0x434E22 je short 00005C17h 
0x434E30 mov eax,[eax+4] - EAX=9172488- mem[9172492]=Ok(5846320) 
0x434E33 ret 4 

0x4969C6 test eax,eax - EAX=5846320- EAX=5846320 
0x4969C8 je short 00017CAEh 
0x4969CA push esi - ESI=1696168 
0x4969CB push 0 
0x4969CD lea edx,[esp+14h] - EDX=1695748- mem[1695868]=Ok(4) 
0x4969D1 push 8BF7ECh 
0x4969D6 push edx - EDX=1695868 
0x4969D7 call eax 

0x593530 push 0FFFFFFFFh 
0x593532 push 7A3BB8h 
0x593537 mov eax,[fs:0] - EAX=5846320- mem[0]=Err(299) 
0x59353D push eax - EAX=1695896 
0x59353E mov [fs:0],esp - mem[0]=Err(299)- ESP=1695824 
0x593545 sub esp,1Ch - ESP=1695824 
0x593548 xor eax,eax - EAX=1695896- EAX=1695896 
0x59354A push esi - ESI=1696168 
0x59354B mov dword [esp+1Ch],0Fh - mem[1695820]=Ok(1696168) 
0x593553 mov [esp+18h],eax - mem[1695816]=Ok(0)- EAX=0 
0x593557 mov [esp+8],al - mem[1695800]=Ok(1695832)- AL=0 
0x59355B mov esi,[esp+3Ch] - ESI=1696168- mem[1695852]=Ok(1696168) 
0x59355F cmp [esi+64h],eax - mem[1696268]=Ok(0)- EAX=0 
0x593562 mov [esp+28h],eax - mem[1695832]=Ok(4294967295)- EAX=0 
0x593566 jne short 0000041Dh 
0x593568 mov ecx,[esi+0Ch] - ECX=20- mem[1696180]=Ok(181321432) 
0x59356B mov eax,[ecx] - EAX=0- mem[181321432]=Ok(8478452) 
0x59356D mov edx,[eax+54h] - EDX=1695868- mem[8478536]=Ok(5190992) 
0x593570 call edx 

0x4F3550 mov al,1 - AL=244 
0x4F3552 ret 

0x593572 test al,al - AL=1- AL=1 
0x593574 je short 0000041Dh 
0x593576 lea eax,[esp+4] - EAX=8478209- mem[1695796]=Ok(8) 
0x59357A push eax - EAX=1695796 
0x59357B jmp short 00000428h 
0x593588 mov ecx,esi - ECX=181321432- ESI=1696168 
0x59358A call 0FFEB7AD0h 

0x44AC30 push 0FFFFFFFFh 
0x44AC32 push 78FF4Ch 
0x44AC37 mov eax,[fs:0] - EAX=1695796- mem[0]=Err(299) 
0x44AC3D push eax - EAX=1695824 
0x44AC3E mov [fs:0],esp - mem[0]=Err(299)- ESP=1695772 
0x44AC45 sub esp,3Ch - ESP=1695772 
0x44AC48 push ebp - EBP=106 
0x44AC49 mov ebp,[esp+50h] - EBP=106- mem[1695788]=Ok(1695796) 
0x44AC4D mov eax,[ebp+14h] - EAX=1695824- mem[1695816]=Ok(0) 
0x44AC50 push esi - ESI=1696168 
0x44AC51 mov esi,ecx - ESI=1696168- ECX=1696168 
0x44AC53 cmp dword [esi+64h],0 - mem[1696268]=Ok(0) 
0x44AC57 mov [esp+54h],eax - mem[1695788]=Ok(1695796)- EAX=0 
0x44AC5B push 4 
0x44AC5D jne short 0000E205h 
0x44AC5F lea eax,[esp+58h] - EAX=0- mem[1695788]=Ok(0) 
0x44AC63 push eax - EAX=1695788 
0x44AC64 call 0000D93Ah 

0x44A3A0 push ebp - EBP=1695796 
0x44A3A1 mov ebp,[esp+0Ch] - EBP=1695796- mem[1695700]=Ok(4) 
0x44A3A5 test ebp,ebp - EBP=4- EBP=4 
0x44A3A7 push esi - ESI=1696168 
0x44A3A8 mov esi,ecx - ESI=1696168- ECX=1696168 
0x44A3AA jne short 0000D94Dh 
0x44A3B3 cmp byte [esi+0A0h],0 - mem[1696328]=Ok(0) 
0x44A3BA je near 0000DAA4h 
0x44A50A mov ecx,[esi+0Ch] - ECX=1696168- mem[1696180]=Ok(181321432) 
0x44A50D mov edx,[ecx] - EDX=5190992- mem[181321432]=Ok(8478452) 
0x44A50F mov eax,[esp+0Ch] - EAX=1695788- mem[1695696]=Ok(1695788) 
0x44A513 mov edx,[edx+20h] - EDX=8478452- mem[8478484]=Ok(7570528) 
0x44A516 push ebp - EBP=4 
0x44A517 push eax - EAX=1695788 
0x44A518 call edx 

0x738460 push esi - ESI=1696168 
0x738461 mov esi,ecx - ESI=1696168- ECX=181321432 
0x738463 xor eax,eax - EAX=1695788- EAX=1695788 
0x738465 cmp [esi+40h],eax - mem[181321496]=Ok(1724)- EAX=0 
0x738468 jne short 0002B994h 
0x73846E cmp [8FA5F0h],al - mem[9414128]=Ok(0)- AL=0 
0x738474 push ebx - EBX=0 
0x738475 mov ebx,[esp+10h] - EBX=0- mem[1695680]=Ok(4) 
0x738479 push ebp - EBP=4 
0x73847A push edi - EDI=1696168 
0x73847B je short 0002B9EAh 
0x7384C4 mov edi,[esp+14h] - EDI=1696168- mem[1695676]=Ok(1695788) 
0x7384C8 mov ebp,[7E1268h] - EBP=4- mem[8262248]=Ok(1975661760) 
0x7384CE mov edi,edi - EDI=1695788- EDI=1695788 
0x7384D0 mov ecx,[esi+40h] - ECX=181321432- mem[181321496]=Ok(1724) 
0x7384D3 push 0 
0x7384D5 lea eax,[esp+1Ch] - EAX=0- mem[1695680]=Ok(4) 
0x7384D9 push eax - EAX=1695680 
0x7384DA push ebx - EBX=4 
0x7384DB push edi - EDI=1695788 
0x7384DC push ecx - ECX=1724 
0x7384DD mov dword [esp+2Ch],0 - mem[1695680]=Ok(4) 
0x7384E5 call ebp 

0x75C234C0 jmp dword [75C81018h] 
{"name":"ReadFile","args":{"hFile":1724,"lpBuffer":1695788,"nNumberOfBytesToRead":4,"lpNumberOfBytesRead":1695680,"lpOverlapped":0}} 
15 1695788 0x19e02c 4 [0,0,0,0] 
0x7384E9 jne short 0002BA1Eh 
0x7384F8 mov eax,[esp+18h] - EAX=1- mem[1695680]=Ok(4) 
0x7384FC add [esi+44h],eax - mem[181321500]=Ok(104)- EAX=4 
0x7384FF pop edi - EDI=1695788 
0x738500 pop ebp - EBP=1975661760 
0x738501 pop ebx - EBX=4 
0x738502 pop esi - ESI=181321432 
0x738503 ret 8 

0x44A51A pop esi - ESI=1696168 
0x44A51B pop ebp - EBP=4 
0x44A51C ret 8 

0x44AC69 jmp short 0000E215h 
0x44AC7B cmp dword [esi+64h],0 - mem[1696268]=Ok(0) 
0x44AC7F jne near 0000E385h 
0x44AC85 cmp dword [esp+54h],0 - mem[1695788]=Ok(0) 
0x44AC8A jbe near 0000E2C6h 
0x44AD2C pop esi - ESI=1696168 
0x44AD2D pop ebp - EBP=1695796 
0x44AD2E mov ecx,[esp+3Ch] - ECX=4248996687- mem[1695772]=Ok(1695824) 
0x44AD32 mov [fs:0],ecx - mem[0]=Err(299)- ECX=1695824 
0x44AD39 add esp,48h - ESP=1695712 
0x44AD3C ret 4 

0x59358F cmp dword [esp+1Ch],10h - mem[1695820]=Ok(15) 
0x593594 mov dword [esp+28h],0FFFFFFFFh - mem[1695832]=Ok(0) 
0x59359C pop esi - ESI=1696168 
0x59359D jb short 0000044Ch 
0x5935AC mov ecx,[esp+1Ch] - ECX=1695824- mem[1695824]=Ok(1695896) 
0x5935B0 mov eax,1 - EAX=4 
0x5935B5 mov [fs:0],ecx - mem[0]=Err(299)- ECX=1695896 
0x5935BC add esp,28h - ESP=1695796 
0x5935BF ret 

0x4969D9 jmp short 00017CC9h 
0x4969F6 add esp,10h - ESP=1695840 
0x4969F9 cmp dword [esp+24h],10h - mem[1695892]=Ok(15) 
0x4969FE mov dword [esp+30h],0FFFFFFFFh - mem[1695904]=Ok(0) 
0x496A06 pop esi - ESI=1696168 
0x496A07 jb short 00017CEDh 
0x496A1A mov ecx,[esp+24h] - ECX=1695896- mem[1695896]=Ok(1696128) 
0x496A1E mov eax,1 - EAX=1 
0x496A23 mov [fs:0],ecx - mem[0]=Err(299)- ECX=1696128 
0x496A2A add esp,30h - ESP=1695860 
0x496A2D ret 

0x49756A jmp short 0001885Ah 
0x497587 add esp,10h - ESP=1695912 
0x49758A lea ecx,[esp+38h] - ECX=1696128- mem[1695984]=Ok(535119246) 
0x49758E push ecx - ECX=1695984 
0x49758F call 0FFFB6933h

0x435660 mov eax,[esp+4] - EAX=1- mem[1695924]=Ok(1695984) 
0x435664 mov ecx,[eax+4] - ECX=1695984- mem[1695988]=Ok(4234516459) 
0x435667 mov edx,[eax] - EDX=1695596- mem[1695984]=Ok(535119246) 
0x435669 push ecx - ECX=4234516459 
0x43566A push edx - EDX=535119246 
0x43566B call 00005C47h 

0x434E60 mov edx,[8BAD18h] - EDX=535119246- mem[9153816]=Ok(9397932) 
0x434E66 mov eax,edx - EAX=1695984- EDX=9397932 
0x434E68 push esi - ESI=181389840 
0x434E69 xor ecx,ecx - ECX=4234516459- ECX=4234516459 
0x434E6B test eax,eax - EAX=9397932- EAX=9397932 
0x434E6D push edi - EDI=1696168 
0x434E6E je short 00005C72h 
0x434E70 mov esi,[esp+10h] - ESI=181389840- mem[1695916]=Ok(4234516459) 
0x434E74 mov edi,[esp+0Ch] - EDI=1696168- mem[1695912]=Ok(535119246) 
0x434E78 cmp edi,[eax+8] - EDI=535119246- mem[9397940]=Ok(3123116033) 
0x434E7B jne short 00005C69h 
0x434E82 mov ecx,eax - ECX=0- EAX=9397932 
0x434E84 mov eax,[eax+24h] - EAX=9397932- mem[9397968]=Ok(9149684) 
0x434E87 test eax,eax - EAX=9149684- EAX=9149684 
0x434E89 jne short 00005C5Fh 
0x434E78 cmp edi,[eax+8] - EDI=535119246- mem[9149692]=Ok(3396427014) 
0x434E7B jne short 00005C69h 
0x434E82 mov ecx,eax - ECX=9397932- EAX=9149684 
0x434E84 mov eax,[eax+24h] - EAX=9149684- mem[9149720]=Ok(9175568) 
0x434E87 test eax,eax - EAX=9175568- EAX=9175568 
0x434E89 jne short 00005C5Fh 
0x434E78 cmp edi,[eax+8] - EDI=535119246- mem[9175576]=Ok(716593239) 
0x434E7B jne short 00005C69h 
0x434E82 mov ecx,eax - ECX=9149684- EAX=9175568 
0x434E84 mov eax,[eax+24h] - EAX=9175568- mem[9175604]=Ok(9378700) 
0x434E87 test eax,eax - EAX=9378700- EAX=9378700 
0x434E89 jne short 00005C5Fh 
0x434E78 cmp edi,[eax+8] - EDI=535119246- mem[9378708]=Ok(1170829070) 
0x434E7B jne short 00005C69h 
0x434E82 mov ecx,eax - ECX=9175568- EAX=9378700 
0x434E84 mov eax,[eax+24h] - EAX=9378700- mem[9378736]=Ok(9351584) 
0x434E87 test eax,eax - EAX=9351584- EAX=9351584 
0x434E89 jne short 00005C5Fh 
0x434E78 cmp edi,[eax+8] - EDI=535119246- mem[9351592]=Ok(3280833677) 
0x434E7B jne short 00005C69h 
0x434E82 mov ecx,eax - ECX=9378700- EAX=9351584 
0x434E84 mov eax,[eax+24h] - EAX=9351584- mem[9351620]=Ok(9402096) 
0x434E87 test eax,eax - EAX=9402096- EAX=9402096 
0x434E89 jne short 00005C5Fh 
0x434E78 cmp edi,[eax+8] - EDI=535119246- mem[9402104]=Ok(4285714048) 
0x434E7B jne short 00005C69h 
0x434E82 mov ecx,eax - ECX=9351584- EAX=9402096 
0x434E84 mov eax,[eax+24h] - EAX=9402096- mem[9402132]=Ok(9379100) 
0x434E87 test eax,eax - EAX=9379100- EAX=9379100 
0x434E89 jne short 00005C5Fh 
0x434E78 cmp edi,[eax+8] - EDI=535119246- mem[9379108]=Ok(2203891694) 
0x434E7B jne short 00005C69h 
0x434E82 mov ecx,eax - ECX=9402096- EAX=9379100 
0x434E84 mov eax,[eax+24h] - EAX=9379100- mem[9379136]=Ok(9352564) 
0x434E87 test eax,eax - EAX=9352564- EAX=9352564 
0x434E89 jne short 00005C5Fh 
0x434E78 cmp edi,[eax+8] - EDI=535119246- mem[9352572]=Ok(535119246)                <- Hash1
0x434E7B jne short 00005C69h 
0x434E7D cmp esi,[eax+0Ch] - ESI=4234516459- mem[9352576]=Ok(4234516459)            <- Hash2
0x434E80 je short 00005C77h 
0x434E90 cmp edx,eax - EDX=9397932- EAX=9352564 
0x434E92 je short 00005C74h 
0x434E94 test ecx,ecx - ECX=9379100- ECX=9379100 
0x434E96 je short 00005C8Bh 
0x434E98 mov edx,[eax+24h] - EDX=9397932- mem[9352600]=Ok(9379052)                  <- 9379052
0x434E9B mov [ecx+24h],edx - mem[9379136]=Ok(9352564)- EDX=9379052 
0x434E9E mov edx,[8BAD18h] - EDX=9379052- mem[9153816]=Ok(9397932)                  <- 9397932
0x434EA4 pop edi - EDI=535119246 
0x434EA5 mov [eax+24h],edx - mem[9352600]=Ok(9379052)- EDX=9397932 
0x434EA8 mov [8BAD18h],eax - mem[9153816]=Ok(9397932)- EAX=9352564 
0x434EAD pop esi - ESI=4234516459 
0x434EAE ret                                                            <- eax = 9352564

0x435670 add esp,8 - ESP=1695912 
0x435673 ret 

0x497594 add esp,4 - ESP=1695924 
0x497597 cmp dword [edi+64h],0 - mem[1696268]=Ok(0) 
0x49759B mov ebp,eax - EBP=106- EAX=9352564 
0x49759D push 2 
0x49759F jne short 00018882h 
0x4975A1 lea edx,[esp+2Ch] - EDX=9397932- mem[1695968]=Ok(4) 
0x4975A5 push edx - EDX=1695968 
0x4975A6 mov ecx,edi - ECX=9379100- EDI=1696168 
0x4975A8 call 0FFFCB673h 
0x44A3A0 push ebp - EBP=9352564 
0x44A3A1 mov ebp,[esp+0Ch] - EBP=9352564- mem[1695924]=Ok(2) 
0x44A3A5 test ebp,ebp - EBP=2- EBP=2 
0x44A3A7 push esi - ESI=181389840 
0x44A3A8 mov esi,ecx - ESI=181389840- ECX=1696168 
0x44A3AA jne short 0000D94Dh 
0x44A3B3 cmp byte [esi+0A0h],0 - mem[1696328]=Ok(0) 
0x44A3BA je near 0000DAA4h 
0x44A50A mov ecx,[esi+0Ch] - ECX=1696168- mem[1696180]=Ok(181321432) 
0x44A50D mov edx,[ecx] - EDX=1695968- mem[181321432]=Ok(8478452) 
0x44A50F mov eax,[esp+0Ch] - EAX=9352564- mem[1695920]=Ok(1695968) 
0x44A513 mov edx,[edx+20h] - EDX=8478452- mem[8478484]=Ok(7570528) 
0x44A516 push ebp - EBP=2 
0x44A517 push eax - EAX=1695968 
0x44A518 call edx

0x738460 push esi - ESI=1696168 
0x738461 mov esi,ecx - ESI=1696168- ECX=181321432 
0x738463 xor eax,eax - EAX=1695968- EAX=1695968 
0x738465 cmp [esi+40h],eax - mem[181321496]=Ok(1724)- EAX=0 
0x738468 jne short 0002B994h 
0x73846E cmp [8FA5F0h],al - mem[9414128]=Ok(0)- AL=0 
0x738474 push ebx - EBX=0 
0x738475 mov ebx,[esp+10h] - EBX=0- mem[1695904]=Ok(2) 
0x738479 push ebp - EBP=2 
0x73847A push edi - EDI=1696168 
0x73847B je short 0002B9EAh 
0x7384C4 mov edi,[esp+14h] - EDI=1696168- mem[1695900]=Ok(1695968) 
0x7384C8 mov ebp,[7E1268h] - EBP=2- mem[8262248]=Ok(1975661760) 
0x7384CE mov edi,edi - EDI=1695968- EDI=1695968 
0x7384D0 mov ecx,[esi+40h] - ECX=181321432- mem[181321496]=Ok(1724) 
0x7384D3 push 0 
0x7384D5 lea eax,[esp+1Ch] - EAX=0- mem[1695904]=Ok(2) 
0x7384D9 push eax - EAX=1695904 
0x7384DA push ebx - EBX=2 
0x7384DB push edi - EDI=1695968 
0x7384DC push ecx - ECX=1724 
0x7384DD mov dword [esp+2Ch],0 - mem[1695904]=Ok(2) 
0x7384E5 call ebp 

0x75C234C0 jmp dword [75C81018h] 
{"name":"ReadFile","args":{"hFile":1724,"lpBuffer":1695968,"nNumberOfBytesToRead":2,"lpNumberOfBytesRead":1695904,"lpOverlapped":0}} 
16 1695968 0x19e0e0 2 [83,0] 
0x7384E9 jne short 0002BA1Eh 
0x7384F8 mov eax,[esp+18h] - EAX=1- mem[1695904]=Ok(2) 
0x7384FC add [esi+44h],eax - mem[181321500]=Ok(108)- EAX=2 
0x7384FF pop edi - EDI=1695968 
0x738500 pop ebp - EBP=1975661760 
0x738501 pop ebx - EBX=2 
0x738502 pop esi - ESI=181321432 
0x738503 ret 8 

0x44A51A pop esi - ESI=1696168 
0x44A51B pop ebp - EBP=2 
0x44A51C ret 8 

0x4975AD jmp short 00018897h 
0x4975C4 cmp dword [edi+64h],0 - mem[1696268]=Ok(0) 
0x4975C8 push 2 
0x4975CA jne short 000188ADh 
0x4975CC lea edx,[esp+1Ch] - EDX=1695820- mem[1695952]=Ok(64362028) 
0x4975D0 push edx - EDX=1695952 
0x4975D1 mov ecx,edi - ECX=4249007279- EDI=1696168 
0x4975D3 call 0FFFCB673h 

0x44A3A0 push ebp - EBP=9352564 
0x44A3A1 mov ebp,[esp+0Ch] - EBP=9352564- mem[1695924]=Ok(2) 
0x44A3A5 test ebp,ebp - EBP=2- EBP=2 
0x44A3A7 push esi - ESI=181389840 
0x44A3A8 mov esi,ecx - ESI=181389840- ECX=1696168 
0x44A3AA jne short 0000D94Dh 
0x44A3B3 cmp byte [esi+0A0h],0 - mem[1696328]=Ok(0) 
0x44A3BA je near 0000DAA4h 
0x44A50A mov ecx,[esi+0Ch] - ECX=1696168- mem[1696180]=Ok(181321432) 
0x44A50D mov edx,[ecx] - EDX=1695952- mem[181321432]=Ok(8478452) 
0x44A50F mov eax,[esp+0Ch] - EAX=2- mem[1695920]=Ok(1695952) 
0x44A513 mov edx,[edx+20h] - EDX=8478452- mem[8478484]=Ok(7570528) 
0x44A516 push ebp - EBP=2 
0x44A517 push eax - EAX=1695952 
0x44A518 call edx 

0x738460 push esi - ESI=1696168 
0x738461 mov esi,ecx - ESI=1696168- ECX=181321432 
0x738463 xor eax,eax - EAX=1695952- EAX=1695952 
0x738465 cmp [esi+40h],eax - mem[181321496]=Ok(1724)- EAX=0 
0x738468 jne short 0002B994h 
0x73846E cmp [8FA5F0h],al - mem[9414128]=Ok(0)- AL=0 
0x738474 push ebx - EBX=0 
0x738475 mov ebx,[esp+10h] - EBX=0- mem[1695904]=Ok(2) 
0x738479 push ebp - EBP=2 
0x73847A push edi - EDI=1696168 
0x73847B je short 0002B9EAh 
0x7384C4 mov edi,[esp+14h] - EDI=1696168- mem[1695900]=Ok(1695952) 
0x7384C8 mov ebp,[7E1268h] - EBP=2- mem[8262248]=Ok(1975661760) 
0x7384CE mov edi,edi - EDI=1695952- EDI=1695952 
0x7384D0 mov ecx,[esi+40h] - ECX=181321432- mem[181321496]=Ok(1724) 
0x7384D3 push 0 
0x7384D5 lea eax,[esp+1Ch] - EAX=0- mem[1695904]=Ok(2) 
0x7384D9 push eax - EAX=1695904 
0x7384DA push ebx - EBX=2 
0x7384DB push edi - EDI=1695952 
0x7384DC push ecx - ECX=1724 
0x7384DD mov dword [esp+2Ch],0 - mem[1695904]=Ok(2) 
0x7384E5 call ebp 

0x75C234C0 jmp dword [75C81018h] 
{"name":"ReadFile","args":{"hFile":1724,"lpBuffer":1695952,"nNumberOfBytesToRead":2,"lpNumberOfBytesRead":1695904,"lpOverlapped":0}} 
17 1695952 0x19e0d0 2 [0,0] 
0x7384E9 jne short 0002BA1Eh 
0x7384F8 mov eax,[esp+18h] - EAX=1- mem[1695904]=Ok(2) 
0x7384FC add [esi+44h],eax - mem[181321500]=Ok(110)- EAX=2 
0x7384FF pop edi - EDI=1695952 
0x738500 pop ebp - EBP=1975661760 
0x738501 pop ebx - EBX=2 
0x738502 pop esi - ESI=181321432 
0x738503 ret 8 

0x44A51A pop esi - ESI=1696168 
0x44A51B pop ebp - EBP=2 
0x44A51C ret 8 

0x4975D8 jmp short 000188C2h 
0x4975EF movzx edx,word [esp+28h] - EDX=1695820- mem[1695968]=Ok(83) 
0x4975F4 imul edx,[ebp+14h] - EDX=83- mem[9352584]=Ok(248) 
0x4975F8 mov eax,[esp+6Ch] - EAX=2- mem[1696036]=Ok(63201312) 
0x4975FC add [esp+1Ch],edx - mem[1695956]=Ok(0)- EDX=20584 
0x497600 mov [eax+ebx*4],ebp - mem[63201312]=Ok(0)- EBP=9352564 
0x497603 movzx ecx,word [esp+28h] - ECX=4249007279- mem[1695968]=Ok(83) 
0x497608 mov edx,[esp+0A0h] - EDX=20584- mem[1696088]=Ok(63201328) 
0x49760F mov [edx+ebx*4],ecx - mem[63201328]=Ok(0)- ECX=83 
0x497612 movzx eax,word [esp+18h] - EAX=63201312- mem[1695952]=Ok(64356352) 
0x497617 mov ecx,[esp+94h] - ECX=83- mem[1696076]=Ok(63201344) 
0x49761E mov [ecx+ebx*4],eax - mem[63201344]=Ok(0)- EAX=0 
0x497621 add ebx,1 - EBX=0 
0x497624 cmp ebx,[esp+10h] - EBX=1- mem[1695944]=Ok(4) 
0x497628 jl near 000187E3h 
0x497510 lea ecx,[esp+38h] - ECX=63201344- mem[1695984]=Ok(535119246) 
0x497514 call 00017983h 

0x4966B0 mov eax,ecx - EAX=0- ECX=1695984 
0x4966B2 mov dword [eax],0 - mem[1695984]=Ok(535119246) 
0x4966B8 mov dword [eax+4],0 - mem[1695988]=Ok(4234516459) 
0x4966BF ret 

0x497519 test dword [8B9408h],20000000h - mem[9147400]=Ok(536870914) 
0x497523 jne short 0001881Eh 
0x49754B push 14h 
0x49754D mov ecx,8B93F8h - ECX=1695984 
0x497552 call 0FFFB60E3h

0x434E10 mov eax,[ecx+20h] - EAX=1695984- mem[9147416]=Ok(9147136) 
0x434E13 test eax,eax - EAX=9147136- EAX=9147136 
0x434E15 je short 00005C12h 
0x434E17 mov ecx,[esp+4] - ECX=9147384- mem[1695924]=Ok(20) 
0x434E1B jmp short 00005C07h 
0x434E20 cmp [eax],ecx - mem[9147136]=Ok(35)- ECX=20 
0x434E22 je short 00005C17h 
0x434E24 mov eax,[eax+8] - EAX=9147136- mem[9147144]=Ok(9147148) 
0x434E27 test eax,eax - EAX=9147148- EAX=9147148 
0x434E29 jne short 00005C07h 
0x434E20 cmp [eax],ecx - mem[9147148]=Ok(4)- ECX=20 
0x434E22 je short 00005C17h 
0x434E24 mov eax,[eax+8] - EAX=9147148- mem[9147156]=Ok(9147160) 
0x434E27 test eax,eax - EAX=9147160- EAX=9147160 
0x434E29 jne short 00005C07h 
0x434E20 cmp [eax],ecx - mem[9147160]=Ok(20)- ECX=20 
0x434E22 je short 00005C17h 
0x434E30 mov eax,[eax+4] - EAX=9147160- mem[9147164]=Ok(4811008) 
0x434E33 ret 4 

0x497557 test eax,eax - EAX=4811008- EAX=4811008 
0x497559 je short 0001883Fh 
0x49755B push edi - EDI=1696168 
0x49755C push 0 
0x49755E lea ecx,[esp+40h] - ECX=20- mem[1695984]=Ok(0) 
0x497562 push 8B93F8h 
0x497567 push ecx - ECX=1695984 
0x497568 call eax

0x496900 mov eax,[fs:0] - EAX=4811008- mem[0]=Err(299) 
0x496906 push 0FFFFFFFFh 
0x496908 push 793410h 
0x49690D push eax - EAX=1696128 
0x49690E mov [fs:0],esp - mem[0]=Err(299)- ESP=1695896 
0x496915 sub esp,24h - ESP=1695896 
0x496918 push esi - ESI=181389840 
0x496919 mov esi,[esp+44h] - ESI=181389840- mem[1695924]=Ok(1696168) 
0x49691D cmp dword [esi+64h],1 - mem[1696268]=Ok(0) 
0x496921 jne short 00017C0Bh 
0x496938 cmp dword [esi+64h],0 - mem[1696268]=Ok(0) 
0x49693C push 8 
0x49693E jne short 00017C21h 
0x496940 mov ecx,[esp+3Ch] - ECX=1695984- mem[1695912]=Ok(1695984) 
0x496944 push ecx - ECX=1695984 
0x496945 mov ecx,esi - ECX=1695984- ESI=1696168 
0x496947 call 0FFFCB673h 

0x44A3A0 push ebp - EBP=9352564 
0x44A3A1 mov ebp,[esp+0Ch] - EBP=9352564- mem[1695852]=Ok(8) 
0x44A3A5 test ebp,ebp - EBP=8- EBP=8 
0x44A3A7 push esi - ESI=1696168 
0x44A3A8 mov esi,ecx - ESI=1696168- ECX=1696168 
0x44A3AA jne short 0000D94Dh 
0x44A3B3 cmp byte [esi+0A0h],0 - mem[1696328]=Ok(0) 
0x44A3BA je near 0000DAA4h 
0x44A50A mov ecx,[esi+0Ch] - ECX=1696168- mem[1696180]=Ok(181321432) 
0x44A50D mov edx,[ecx] - EDX=63201328- mem[181321432]=Ok(8478452) 
0x44A50F mov eax,[esp+0Ch] - EAX=1696128- mem[1695848]=Ok(1695984) 
0x44A513 mov edx,[edx+20h] - EDX=8478452- mem[8478484]=Ok(7570528) 
0x44A516 push ebp - EBP=8 
0x44A517 push eax - EAX=1695984 
0x44A518 call edx 

0x738460 push esi - ESI=1696168 
0x738461 mov esi,ecx - ESI=1696168- ECX=181321432 
0x738463 xor eax,eax - EAX=1695984- EAX=1695984 
0x738465 cmp [esi+40h],eax - mem[181321496]=Ok(1724)- EAX=0 
0x738468 jne short 0002B994h 
0x73846E cmp [8FA5F0h],al - mem[9414128]=Ok(0)- AL=0 
0x738474 push ebx - EBX=1 
0x738475 mov ebx,[esp+10h] - EBX=1- mem[1695832]=Ok(8) 
0x738479 push ebp - EBP=8 
0x73847A push edi - EDI=1696168 
0x73847B je short 0002B9EAh 
0x7384C4 mov edi,[esp+14h] - EDI=1696168- mem[1695828]=Ok(1695984) 
0x7384C8 mov ebp,[7E1268h] - EBP=8- mem[8262248]=Ok(1975661760) 
0x7384CE mov edi,edi - EDI=1695984- EDI=1695984 
0x7384D0 mov ecx,[esi+40h] - ECX=181321432- mem[181321496]=Ok(1724) 
0x7384D3 push 0 
0x7384D5 lea eax,[esp+1Ch] - EAX=0- mem[1695832]=Ok(8) 
0x7384D9 push eax - EAX=1695832 
0x7384DA push ebx - EBX=8 
0x7384DB push edi - EDI=1695984 
0x7384DC push ecx - ECX=1724 
0x7384DD mov dword [esp+2Ch],0 - mem[1695832]=Ok(8) 
0x7384E5 call ebp 
