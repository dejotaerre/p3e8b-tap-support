#!/usr/bin/env php
<?php

define("TAB",chr(9));

$vars          = get_vars48k();
$nemonicos     = get_nemonicos();
$substs        = get_substs();
$registros     = get_registros();
$otros         = get_otros();
$digits        = get_digits();
$finales       = get_finales();
$directivas    = get_directivas();
$revertir      = get_revertir();
$registros_ext = get_registros_ext();

$labels = [];

if (file_exists("labels.txt"))
{

	$lineas = explode(chr(10),file_get_contents("labels.txt"));
	foreach ($lineas as $linea)
	{
		$aux   = explode(" ",$linea);
		$label = $aux[sizeof($aux)-1];
		if ($label!="") $labels[] = $label;
	}

}

$start_time = microtime(true);

	//-------------------------------------------------------------------------------

	$roms = [0,1,2,3];
	//$roms = [1];

	foreach ($roms as $valor) 
	{
		$origen  = "oR${valor}.asm";				// nombre del archivo a procesar
		$destino = "plus3ROM${valor}.asm";			// nombre de archivo para el resultado
		$result  = beautificar($origen,$valor);
		file_put_contents($destino, $result);
	}

	//-------------------------------------------------------------------------------

$end_time  = microtime(true);

$elapsed_time = $end_time - $start_time;

echo "\nbeautificación tardó ".round($elapsed_time,0)." segs\n\n";

exit(0);

// ===================================================================================================
// ===================================================================================================
// ===================================================================================================

function beautificar($origen,$nrom)
{

	global $vars,$nemonicos,$substs,$registros,$otros,$digits,$finales,$directivas,$revertir,$labels;

	$cont = file_get_contents($origen);						//archivo a procesar
	$cont = str_ireplace("';'"  , "$3B" ,$cont);	//Y DE ENTRADA NOMAS... FUNDAMENTAL PARA NO
	$cont = str_ireplace("\";\"", "$3B" ,$cont);	//VOLVERME LOCO BUSCANDO COMENTARIOS... LPM!

	foreach ($vars as $indice => $valor) 
	{
		// SUSTITUCION DE VARIABLES DEL SISTEMA POR LABELS MAS AMIGABLES
		$cont = str_ireplace("($valor)","($indice)",$cont);
		$cont = str_ireplace(",$valor",",$indice",$cont);
	}

	foreach ($digits as $valor) 
	{
		// ODIO DIGITOS Y OTROS SIMBOLOS ENCERRADOS EN COMILLAS SIMPLES
		$buscar = "'" . $valor . "'";
		$sust   = "\"" . $valor . "\"";
		$cont   = str_replace($buscar,$sust,$cont);
	}

	for ($decimal = 0; $decimal <= 65535; $decimal++) 
	{
		// ODIO NUMEROS HEXA DE 16 BITS SIN PADDINGS CON 0'S
		$hex  = "$".strtoupper(str_pad(dechex($decimal),4,"0",STR_PAD_LEFT));
		$cont = str_ireplace($hex,$hex,$cont);
	}

	for ($decimal = 0; $decimal <= 255; $decimal++) 
	{
		// ODIO NUMEROS HEXA DE 8 BITS SIN PADDINGS CON 0'S
		$hex  = "$".strtoupper(str_pad(dechex($decimal),2,"0",STR_PAD_LEFT));
		$cont = str_ireplace($hex,$hex,$cont);
	}

	$lineas = explode(chr(10),str_replace(chr(13),"",$cont));

	$comentarios1   = [];	//comentarios de una sola linea (o sea es lo unico que tiene)
	$comentarios2   = [];	//comentarios al final de una linea
	$sin_etiqueta   = [];	//linea normal sin etiqueta a la que se le quitó el comentario al final si lo tenia
	$con_etiqueta   = [];	//el contenido de una linea sin etiqueta al principio ni comentario al final
	$etiquetas      = [];	//etiqueta al principio (le agrego dos puntos si no lo tiene)

	foreach ($lineas as $indice => $linea) {

		$linea=str_replace(TAB," ",$linea);

		if (substr(trim($linea),0,1)==";")
		{
			//linea solo comentario debe quedar tal cual
			$comentarios1[$indice]=trim($linea);
		}
		elseif (trim($linea)=="")
		{
			//linea en blanco
			$en_blanco[$indice]="";
		}
		elseif (substr($linea,0,1)==" ")
		{
			//linea que NO empieza con un label y puede contener comentario
			
			$pos=strpos($linea,";");
			if ($pos!==false)
			{
				//aislo comentario
				$comment=substr($linea,$pos);
				if (substr($comment,0,2)==";;") $comment=substr($comment,1);
				if (substr($comment,1,1)!=" ") $comment="; ".substr($comment,1);
				$comentarios2[$indice]=trim($comment);
				$linea=substr($linea,0,$pos);
			}

			$linea = trim($linea);

			$sin_etiqueta[$indice]=$linea;

		}
		else
		{
			//linea que SI empieza con un label y puede contener comentario
			$pos=strpos($linea,";");
			if ($pos!==false)
			{
				//aislo comentario
				$comment=substr($linea,$pos);
				if (substr($comment,0,2)==";;") $comment=substr($comment,1);
				if (substr($comment,1,1)!=" ") $comment="; ".substr($comment,1);
				$comentarios2[$indice]=trim($comment);
				$linea=substr($linea,0,$pos);
			}

			$linea=trim($linea);

			$aux=explode(" ",$linea);

			//$etiquetas[$indice]=trim($aux[0]);
			if (!isset( $directivas[strtolower($aux[0])]))
			{
				$etiquetas[$indice]=str_replace(":","",trim($aux[0])).":";
			}
			else
			{
				/* por si algun pelotudo puso una directiva al principio de la línea*/
				$etiquetas[$indice]=chr(9).chr(9).trim($aux[0]);
			}

			unset($aux[0]);

			$linea=implode(" ",$aux);
			$linea=trim($linea);

			$con_etiqueta[$indice]=$linea;

		}

	}

	$nuevas_lineas = [];

	$last_len_comment = 0;

	$con_comentario_anterior = false;

	for ($indice=0;$indice<=sizeof($lineas);$indice++)
	{

		$nl = "";

		if (isset($comentarios1[$indice]))
		{
			//linea con solo comentario empieza al principio
			if ($con_comentario_anterior)
			{
				$nl = str_repeat(TAB,5).$comentarios1[$indice];
			}
			else 
			{
				$nl = $comentarios1[$indice];
			}
		}
		else
		{

			$con_comentario_anterior = false;

			if (isset($etiquetas[$indice]))
			{
				//linea que comienza con etiqueta
				$nl = $etiquetas[$indice];
			}

			if (isset($con_etiqueta[$indice]))
			{

				//contenido de linea que se agrega a la etiqueta
				$tab = ($nl=="") ? str_repeat(TAB,3) : str_repeat(TAB,2);

				//if (strlen($nl)>8) $tab = TAB;

				$nl .= $tab.tratar_linea($con_etiqueta[$indice],$indice);

			} 
			else if (isset($sin_etiqueta[$indice]))
			{

				//linea que no comienza con etiqueta
				$nl = str_repeat(TAB,2) . tratar_linea($sin_etiqueta[$indice],$indice);

			}

			if (isset($comentarios2[$indice]))
			{
				//linea que tiene un comentario al final
				$nl .= $tab.$comentarios2[$indice];
				$con_comentario_anterior = true;

			}

			if (isset($con_etiqueta[$indice]))
			{

				$aux = explode(TAB,$nl);

				$nwc = [];

				foreach ($aux as $valor) if ($valor!="") $nwc[]=$valor;

				if (isset($comentarios2[$indice])) unset($nwc[sizeof($nwc)-1]);

				if (strlen($nwc[0])<8 || isset($con_etiqueta[$indice])) 
				{
					$nlw = $nwc[0] . TAB . TAB;
				}
				else
				{
					$nlw = $nwc[0] . TAB;
				}

				if (isset($nwc[1])) $nlw .= $nwc[1] . TAB;
				if (isset($nwc[2])) $nlw .= $nwc[2];

				if (isset($comentarios2[$indice]) && isset($nwc[2]))
				{
					if (strlen($nwc[2])<8)
					{
						$nlw .= TAB . TAB . $comentarios2[$indice];
					}
					else
					{
						$nlw .= TAB . $comentarios2[$indice];
					}

				}

				if (sizeof($nwc)>3)
				{
					for ($i=3;$i<=sizeof($nwc);$i++)
					{
						$nlw .= TAB . $nwc[$i];
					}
				}

				$nl = remover_tabs_final($nlw);

			}
			else if (isset($comentarios2[$indice]))
			{
				//sin etiqueta pero con comentario
				$aux = explode(TAB,$nl);

				$nwc = [];

				foreach ($aux as $valor) if ($valor!="") $nwc[]=$valor;

				unset($nwc[sizeof($nwc)-1]);

				$nlw = TAB . TAB;

				if (isset($nwc[0]))
				{

					$nlw .= $nwc[0] . TAB;

					if (isset($nwc[1]))
					{

						$nlw .= $nwc[1];

						if (isset($nwc[2]))
						{

							$nlw .= TAB . $nwc[2];

							if (strlen($nwc[2])<8)
							{
								$nlw .= TAB;
							}

						}
						else
						{
							if (strlen($nwc[1])<8)
							{
								$nlw .= TAB;
							}
						}

					}
					else
					{

						$nlw .= TAB;

					}

				}

				$nlw .= TAB . $comentarios2[$indice];

				$nl = remover_tabs_final($nlw);

			}

		}

		foreach ($nemonicos as $valor) 
		{

			$buscar = TAB . $valor .TAB;
			$sust   = TAB . strtoupper($valor) . TAB;
			$nl     = str_ireplace($buscar, $sust, $nl);

			$sp = 1+strlen($valor);
			if (substr($nl,-$sp)==chr(9).$valor)
			{
				$buscar = TAB . $valor;
				$sust   = TAB . strtoupper($valor);
				$nl = str_ireplace($buscar, $sust, $nl);
			}

			$buscar = TAB . $valor;
			$sust   = TAB . strtoupper($valor);
			$nl     = str_ireplace($buscar, $sust, $nl);

		}

		foreach ($finales as $valor) 
		{
			$buscar = $valor[0];
			$sust   = $valor[1];
			$nl     = str_ireplace($buscar, $sust, $nl);
		}

		$nl = str_replace(TAB."DB($",TAB."DB".TAB."($",$nl); 	//reemplazo de ultimo momento. no entiendo por que

		$nuevas_lineas[]=$nl;

		//echo $nl."\n";

	}

	$asm = implode(chr(10),$nuevas_lineas);

	if ($nrom!=3)
	{
		/*
		ESTO ES NECESARIO EN ESPECIAL PARA LA ROM1 POR QUE SU DESENSAMBLE ES UN DESASTRE, ESTA LLENO DE LLAMADAS
		A ROM3 CON DIRECCIONES ABSOLUTAS EN VEZ DE LABELS

		ASI QUE GENERE UN ARCHIVO CON TODOS LOS LABELS DE LA ROM3 Y LO PARSEO PARA FORMAR UN ARRAY CON ELLOS
		Y LABEL POR LABEL LA BUSCO Y SUSTITUYO
		
		EJ: BUSCO $1C82 Y REEMPLAZO POR o1C82 SI ESTA EN EL ARRAY
		*/
		
		foreach ($labels as $label)
		{
			$buscar = "\$".substr($label,-4);
			$sust   = "o".substr($label,-4);
			$asm    = str_replace($buscar,$sust,$asm);
		}
	
	}

	while (substr($asm,-1)==chr(10))
		$asm = substr($asm,0,strlen($asm)-1);

	/*
	¡NOTA!
	en la ROM2 tuve que modificar la línea de la etiqueta l0bf9
	por que me resultó imposible de conciliar con la beautificación
	(por el punto y coma que contiene el string)
	*/

	return $asm.chr(10);

}

// =======================================================
// ********************** FINAL **************************
// =======================================================

function tratar_linea($linea,$nlinea)
{

	global $substs,$registros,$otros,$revertir,$registros_ext;

	//busco comillas dobles o digits y parto la línea si están
	$resto = "";
	$pos   = strpos($linea,"\"");

	if ($pos!==false)
	{
		$resto = trim(substr($linea,$pos));
		$linea = substr($linea,0,$pos);
	}
	else
	{
		$pos = strpos($linea,"'");
		if ($pos!==false)
		{
			$resto = trim(substr($linea,$pos));
			$linea = substr($linea,0,$pos);
		}
	}

	foreach ($substs as $valor)
	{

		$buscar = $valor[0];
		$sust   = $valor[1];
		$linea  = str_ireplace($buscar,$sust,$linea);

	}

	foreach ($revertir as $valor)
	{

		$buscar = $valor[0];
		$sust   = $valor[1];
		$linea  = str_ireplace($buscar,$sust,$linea);

	}

	//reduzco a un espacio de separacion cada "palabra" de linea limpia
	$pos=strpos($linea,"  ");
	while ($pos!==false)
	{
		$linea=str_replace("  "," ",$linea);
		$pos=strpos($linea,"  ");
	}

	$aux = explode(" ",$linea);
	if (sizeof($aux)>1) 
	{
		$linea = (strtolower($aux[0])=="if") ? $aux[0]." " : $aux[0].TAB;
		unset($aux[0]);
		$linea .= implode(" ",$aux);
	}

	foreach ($registros as $ndx => $valor)
	{
		if ( strtolower( substr($linea,-(strlen($valor)+1)) ) == strtolower(",$valor") )
		{
			$linea = substr($linea,0,strlen($linea)-(strlen($valor)+1)) . ",". strtoupper($valor);
		}
		else if ( strtolower( substr($linea,-(strlen($valor)+1)) ) == strtolower(TAB.$valor) )
		{
			$linea = substr($linea,0,strlen($linea)-(strlen($valor)+1)) . TAB . strtoupper($valor);
		}
		else if ( strtolower( substr($linea,-(strlen($valor)+2)) ) == strtolower(", ".$valor) )
		{
			$linea = substr($linea,0,strlen($linea)-(strlen($valor)+2)) . "," . strtoupper($valor);
		}
	
	}

	foreach ($registros as $valor)
	{
		$buscar = ", ($valor)";
		$sust   = ",($valor)";
		$linea  = str_ireplace($buscar,$sust,$linea);

		$buscar = "$valor, (";
		$sust   = "$valor,(";
		$linea  = str_ireplace($buscar,$sust,$linea);

		$buscar = " $valor,";
		$sust   = " $valor,";
		$linea  = str_ireplace($buscar,$sust,$linea);

		$buscar = TAB."$valor,";
		$sust   = TAB."$valor,";
		$linea  = str_ireplace($buscar,$sust,$linea);

		$buscar = " ($valor)";
		$sust   = " ($valor)";
		$linea  = str_ireplace($buscar,$sust,$linea);

		$buscar = TAB."($valor)";
		$sust   = TAB."($valor)";
		$linea  = str_ireplace($buscar,$sust,$linea);

		$buscar = "($valor)";
		$sust   = "($valor)";
		$linea  = str_ireplace($buscar,$sust,$linea);

		$buscar = " ($valor+";
		$sust   = " ($valor+";
		$linea  = str_ireplace($buscar,$sust,$linea);

		$buscar = TAB."($valor+";
		$sust   = TAB."($valor+";
		$linea  = str_ireplace($buscar,$sust,$linea);

		$buscar = " ($valor-";
		$sust   = " ($valor-";
		$linea  = str_ireplace($buscar,$sust,$linea);

		$buscar = TAB."($valor-";
		$sust   = TAB."($valor-";
		$linea  = str_ireplace($buscar,$sust,$linea);

	}

	foreach ($registros_ext as $valor)
	{

		$buscar = " $valor";
		$sust   = " $valor";
		$linea  = str_ireplace($buscar,$sust,$linea);

		$buscar = TAB."$valor";
		$sust   = TAB."$valor";
		$linea  = str_ireplace($buscar,$sust,$linea);

	}

	foreach ($otros as $ndx => $valor)
	{

		if ( strtolower($linea) == strtolower($valor) )
		{
			$linea = strtoupper($valor);
		}
		else if ( strtolower( substr($linea,-(strlen($valor)+1)) ) == TAB . strtolower($valor) )
		{
			$linea = substr($linea,0,strlen($linea)-(strlen($valor)+1)) . TAB . strtoupper($valor);
		}

	}

	if ($resto!="")
	{
		for ($i=65;$i<=90;$i++)
		{
			$resto = str_replace( "'".chr($i)."'", "\"".chr($i)."\"" ,$resto );
		}

		for ($i=97;$i<=122;$i++)
		{
			$resto = str_replace( "'".chr($i)."'", "\"".chr($i)."\"" ,$resto );
		}

		$linea .= $resto;

		$linea = str_ireplace( "db\"", "DB".TAB."\"", $linea );
		$linea = str_ireplace( "db'" , "DB".TAB."'" , $linea );
		$linea = str_ireplace( "dm\"", "DM".TAB."\"", $linea );
		$linea = str_ireplace( "dm'" , "DM".TAB."'" , $linea );

		$linea = str_ireplace( "include\"" , "INCLUDE".TAB."\"" , $linea );
		$linea = str_ireplace( "include'"  , "INCLUDE".TAB."'"  , $linea );
	}

	$buscar = "','";
	$sust   = "\",\"";
	$linea  = str_ireplace($buscar,$sust,$linea);

	$buscar = "(ix";
	$sust   = "(IX";
	$linea  = str_ireplace($buscar,$sust,$linea);

	$buscar = "(iy";
	$sust   = "(IY";
	$linea  = str_ireplace($buscar,$sust,$linea);

	return $linea;

}

function get_registros_ext() {

	$ret = [];
	$ret[] = "IXH";
	$ret[] = "IXL";
	$ret[] = "IYH";
	$ret[] = "IYL";

	return $ret;

}

function get_registros() {

	$ret = [];
	$ret[] = "HL";
	$ret[] = "DE";
	$ret[] = "BC";
	$ret[] = "AF";
	$ret[] = "AF'";
	$ret[] = "IX";
	$ret[] = "IY";
	$ret[] = "SP";
	$ret[] = "A";
	$ret[] = "B";
	$ret[] = "C";
	$ret[] = "D";
	$ret[] = "E";
	$ret[] = "H";
	$ret[] = "L";

	return $ret;

}

function get_vars48k()
{
	$vars=[];
	$vars["KSTATE"]="$5c00";
	$vars["LAST_K"]="$5c08";
	$vars["REPDEL"]="$5c09";
	$vars["REPPER"]="$5c0a";
	$vars["DEFADD"]="$5c0b";
	$vars["K_DATA"]="$5c0d";
	$vars["TVDATA"]="$5c0e";
	$vars["STRMS"]="$5c10";
	$vars["CHARS"]="$5c36";
	$vars["RASP"]="$5c38";
	$vars["PIP"]="$5c39";
	$vars["ERR_NR"]="$5c3a";
	$vars["FLAGS"]="$5c3b";
	$vars["TV_FLAG"]="$5c3c";
	$vars["ERR_SP"]="$5c3d";
	$vars["LIST_SP"]="$5c3f";
	$vars["MODE"]="$5c41";
	$vars["NEWPPC"]="$5c42";
	$vars["NSPPC"]="$5c44";
	$vars["PPC"]="$5c45";
	$vars["SUBPPC"]="$5c47";
	$vars["BORDCR"]="$5c48";
	$vars["E_PPC"]="$5c49";
	$vars["VARS"]="$5c4b";
	$vars["DEST"]="$5c4d";
	$vars["CHANS"]="$5c4f";
	$vars["CURCHL"]="$5c51";
	$vars["PROG"]="$5c53";
	$vars["NXTLIN"]="$5c55";
	$vars["DATADD"]="$5c57";
	$vars["E_LINE"]="$5c59";
	$vars["K_CUR"]="$5c5b";
	$vars["CH_ADD"]="$5c5d";
	$vars["X_PTR"]="$5c5f";
	$vars["WORKSP"]="$5c61";
	$vars["STKBOT"]="$5c63";
	$vars["STKEND"]="$5c65";
	$vars["BREG"]="$5c67";
	$vars["MEM"]="$5c68";
	$vars["FLAGS2"]="$5c6a";
	$vars["DF_SZ"]="$5c6b";
	$vars["S_TOP"]="$5c6c";
	$vars["OLDPPC"]="$5c6e";
	$vars["OSPCC"]="$5c70";
	$vars["FLAGX"]="$5c71";
	$vars["STRLEN"]="$5c72";
	$vars["T_ADDR"]="$5c74";
	$vars["SEED"]="$5c76";
	$vars["FRAMES"]="$5c78";
	$vars["UDG"]="$5c7b";
	$vars["COORDS"]="$5c7d";
	$vars["P_POSN"]="$5c7f";
	$vars["PR_CC"]="$5c80";
	$vars["ECHO_E"]="$5c82";
	$vars["DF_CC"]="$5c84";
	$vars["DF_CCL"]="$5c86";
	$vars["S_POSN"]="$5c88";
	$vars["SPOSNL"]="$5c8a";
	$vars["SCR_CT"]="$5c8c";
	$vars["ATTR_P"]="$5c8d";
	$vars["MASK_P"]="$5c8e";
	$vars["ATTR_T"]="$5c8f";
	$vars["MASK_T"]="$5c90";
	$vars["P_FLAG"]="$5c91";
	$vars["MEMBOT"]="$5c92";
	$vars["NMIADD"]="$5cb0";
	$vars["RAMTOP"]="$5cb2";
	$vars["P_RAMT"]="$5cb4";

	$vars["SWAP"]="$5B00";
	$vars["STOO"]="$5B10";
	$vars["YOUNGER"]="$5B21";
	$vars["REGNUOY"]="$5B2A";
	$vars["ONERR"]="$5B3A";
	$vars["OLDHL"]="$5B52";
	$vars["OLDBC"]="$5B54";
	$vars["OLDAF"]="$5B56";
	$vars["TARGET"]="$5B58";
	$vars["RETADDR"]="$5B5A";
	$vars["BANKM"]="$5B5C";
	$vars["RAMRST"]="$5B5D";
	$vars["RAMERR"]="$5B5E";
	$vars["BAUD"]="$5B5F";
	$vars["SERFL"]="$5B61";
	$vars["COL"]="$5B63";
	$vars["WIDTH"]="$5B64";
	$vars["TVPARS"]="$5B65";
	$vars["FLAGS3"]="$5B66";
	$vars["BANK678"]="$5B67";
	$vars["XLOC"]="$5B68";
	$vars["YLOC"]="$5B69";
	$vars["OLDSP"]="$5B6A";
	$vars["SYNRET"]="$5B6C";
	$vars["LASTV"]="$5B6E";
	$vars["RC_LINE"]="$5B73";
	$vars["RC_START"]="$5B75";
	$vars["RC_STEP"]="$5B77";
	$vars["LODDRV"]="$5B79";
	$vars["SAVDRV"]="$5B7A";
	$vars["DUMPLF"]="$5B7B";
	$vars["STRIP1"]="$5B7C";
	$vars["STRIP2"]="$5B84";
	$vars["TSTACK"]="$5BFF";
	$vars["PBANKM"]="$7FFD";
	$vars["PBANK678"]="$1FFD";

	return $vars;

}


function get_nemonicos() 
{

	$ret = [];
	$ret[]="ADC";
	$ret[]="ADC";
	$ret[]="ADD";
	$ret[]="AND";
	$ret[]="BIT";
	$ret[]="CALL";
	$ret[]="CCF";
	$ret[]="CP";
	$ret[]="CPD";
	$ret[]="CPDR";
	$ret[]="CPI";
	$ret[]="CPIR";
	$ret[]="CPL";
	$ret[]="DAA";
	$ret[]="DEC";
	$ret[]="DI";
	$ret[]="DJNZ";
	$ret[]="EI";
	$ret[]="EX";
	$ret[]="EXX";
	$ret[]="HALT";
	$ret[]="IM";
	$ret[]="IN";
	$ret[]="INC";
	$ret[]="IND";
	$ret[]="INDR";
	$ret[]="INI";
	$ret[]="INIR";
	$ret[]="JP";
	$ret[]="JR";
	$ret[]="LD";
	$ret[]="LDD";
	$ret[]="LDDR";
	$ret[]="LDI";
	$ret[]="LDIR";
	$ret[]="NEG";
	$ret[]="NOP";
	$ret[]="OR";
	$ret[]="OTDR";
	$ret[]="OTIR";
	$ret[]="OUT";
	$ret[]="OUTD";
	$ret[]="OUTI";
	$ret[]="POP";
	$ret[]="PUSH";
	//$ret[]="RES";
	$ret[]="RET";
	$ret[]="RETI";
	$ret[]="RETN";
	$ret[]="RL";
	$ret[]="RLA";
	$ret[]="RLC";
	$ret[]="RLCA";
	$ret[]="RLD";
	$ret[]="RR";
	$ret[]="RRA";
	$ret[]="RRC";
	$ret[]="RRCA";
	$ret[]="RRD";
	$ret[]="RST";
	$ret[]="SBC";
	$ret[]="SCF";
	$ret[]="SET";
	$ret[]="SLA";
	$ret[]="SRA";
	$ret[]="SRL";
	$ret[]="SUB";
	$ret[]="XOR";

	$ret[]="IF";				//OTROS ""NEMONICOS""
	$ret[]="ELSE";
	$ret[]="ENDIF";
	$ret[]="INCLUDE";
	$ret[]="DEFINE";
	$ret[]="ORG";
	$ret[]="OUTPUT";

	return $ret;

}

function get_substs() 
{

	$ret = [];

	$ret[] = [	".db "		,"DB "	];	//aberraciones encontradas

	$ret[] = [	"defw "		,"DW "	];
	$ret[] = [	"defs "		,"DS "	];
	$ret[] = [	"defm "		,"DM "	];
	$ret[] = [	"defb "		,"DB"		];

	$ret[] = [	" - "		,"-"	];
	$ret[] = [	" + "		,"+"	];
	$ret[] = [	", $"		,",$"	];
	$ret[] = [	", 0"		,",0"	];
	$ret[] = [	", 1"		,",1"	];
	$ret[] = [	", 2"		,",2"	];
	$ret[] = [	", 3"		,",3"	];
	$ret[] = [	", 4"		,",4"	];
	$ret[] = [	", 5"		,",5"	];
	$ret[] = [	", 6"		,",6"	];
	$ret[] = [	", 7"		,",7"	];
	$ret[] = [	", 8"		,",8"	];
	$ret[] = [	", 9"		,",9"	];

	$ret[] = [	"nc, "		," NC,"	];
	$ret[] = [	"nz, "		," NZ,"	];

	$ret[] = [	"nc,"		," NC,"	];
	$ret[] = [	"nz,"		," NZ,"	];

	$ret[] = [	" c,"		," C,"	];
	$ret[] = [	" z,"		," Z,"	];

	$ret[] = [	TAB."c,"		," C,"	];
	$ret[] = [	TAB."z,"		," Z,"	];

	$ret[] = [	"hl, "		,"HL,"	];	
	$ret[] = [	"bc, "		,"BC,"	];
	$ret[] = [	"de, "		,"DE,"	];
	$ret[] = [	"ix, "		,"IX,"	];
	$ret[] = [	"iy, "		,"IY,"	];

	$ret[] = [	" a,"		," A,"	];
	$ret[] = [	" i,"		," I,"	];
	$ret[] = [	" b,"		," B,"	];
	$ret[] = [	" c,"		," C,"	];
	$ret[] = [	" d,"		," D,"	];
	$ret[] = [	" e,"		," E,"	];
	$ret[] = [	" h,"		," H,"	];
	$ret[] = [	" l,"		," L,"	];

	$ret[] = [	"z, "		,"Z,"	];
	$ret[] = [	"nz, "	,"NZ,"	];
	$ret[] = [	"c, "		,"C,"	];
	$ret[] = [	"nc, "	,"NC,"	];

	$ret[] = [	"(sp) "		,"(SP)"	];
	$ret[] = [	"(hl) "		,"(HL)"	];
	$ret[] = [	"(bc) "		,"(BC)"	];
	$ret[] = [	"(de) "		,"(DE)"	];
	$ret[] = [	"(ix) "		,"(IX)"	];
	$ret[] = [	"(iy) "		,"(IY)"	];
	$ret[] = [	"(a) "		,"(A)"	];
	$ret[] = [	"(c) "		,"(C)"	];

	$ret[] = [	"(c) "		,"(C)"	];
	$ret[] = [	"(c) "		,"(C)"	];

	$ret[] = [	"(ix+ "		,"(IX+"	];
	$ret[] = [	"(iy+ "		,"(IY+"	];
	$ret[] = [	"(ix- "		,"(IX-"	];
	$ret[] = [	"(iy- "		,"(IY-"	];

	$ret[] = [	"0, "		,"0,"	];

	return $ret;

}

function get_otros() {

	$ret = [];
	$ret[] = "ret";
	$ret[] = "halt";
	$ret[] = "retn";
	$ret[] = "di";
	$ret[] = "ei";
	$ret[] = "z";
	$ret[] = "n";
	$ret[] = "nz";
	$ret[] = "nc";

	return $ret;

}

function get_finales()
{
	$ret = [];
	$ret[] = [ "RST" . TAB . "$00", "RST" . TAB . "00H" ];	//ME GUSTA VER "RST 10H" EN VEZ DE "RST $10"
	$ret[] = [ "RST" . TAB . "$08", "RST" . TAB . "08H" ];
	$ret[] = [ "RST" . TAB . "$10", "RST" . TAB . "10H" ];
	$ret[] = [ "RST" . TAB . "$18", "RST" . TAB . "18H" ];
	$ret[] = [ "RST" . TAB . "$20", "RST" . TAB . "20H" ];
	$ret[] = [ "RST" . TAB . "$28", "RST" . TAB . "28H" ];
	$ret[] = [ "RST" . TAB . "$30", "RST" . TAB . "30H" ];
	$ret[] = [ "RST" . TAB . "$38", "RST" . TAB . "38H" ];
	return $ret;
}

function get_digits()
{
	$ret = [];
	$ret[] = " ";
	$ret[] = "0";
	$ret[] = "1";
	$ret[] = "2";
	$ret[] = "3";
	$ret[] = "4";
	$ret[] = "5";
	$ret[] = "6";
	$ret[] = "7";
	$ret[] = "8";
	$ret[] = "9";

	$ret[] = ":";			//otras susticiones similares
	$ret[] = "(";
	$ret[] = ")";
	$ret[] = "#";
	$ret[] = ">";
	$ret[] = "<";
	$ret[] = "/";
	$ret[] = "\$";
	$ret[] = "*";
	$ret[] = "=";
	$ret[] = "?";

	return $ret;
}

function get_directivas() {

	$ret = [];
	$ret["org"   ] = 1;
	$ret["if"    ] = 1;
	$ret["else"  ] = 1;
	$ret["endif" ] = 1;
	$ret["define"] = 1;
	return $ret;

}

function get_revertir()
{
	$ret = [];
	$ret[] = [	"iyH,"		,"IYH,"	];
	$ret[] = [	"iyL,"		,"IYL,"	];
	$ret[] = [	"ixH,"		,"IXH,"	];
	$ret[] = [	"ixL,"		,"IXL,"	];
	return $ret;
}

function remover_tabs_final($s)
{
	while (substr($s,-1)==TAB) $s=substr($s,0,strlen($s)-1);
	while (substr($s,-1)==" ") $s=substr($s,0,strlen($s)-1);
	return $s;
}