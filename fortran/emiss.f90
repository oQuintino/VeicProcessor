program gera

use iso_fortran_env, dp => real128

implicit none

integer :: i, j, ispec, ihr, isrc, n, k

integer, parameter :: cx = 159, cy = 109, cz = 3, &                                               ! modificar
  nx = cx, ny = cy, nz = cz - 1, NRADM = 32, nsap99 = 13

real(dp), dimension(0:23) :: hrsplt_co, hrsplt_nox, hrsplt_mp, hrsplt_voc

real(dp), dimension(cx, cy) :: veic_co, veic_co2, veic_ch4, termo_co, termo2_co, termo3_co, &
  refinaria_co, trem_co, trem_co2, trem_ch4, veic, veicfc, veic1, veic2, veic3, veic4a, veic4b, &
  veic4c, veic5, veic6, termo, termo2, termo3, refina, ferro

real(dp), dimension(cx, cy) :: perfil_diurno_veic_co, perfil_diurno_veic_co2, &
  perfil_diurno_veic_ch4

real(dp), dimension(0:23, cx, cy) :: perfis_diurnos_veic_co, perfis_diurnos_veic_co2, &
  perfis_diurnos_veic_ch4

character(len=8) :: snam(13)

! Variaveis para formato WRF/Chem
real(dp), dimension(nx, ny, nz, 0:23) :: e_co, e_co2, e_ch4

!***********saida do dados************
integer :: cont, cont1, cont2, cont3
character(len=9) :: ename(3)
real(dp) :: EM3RD(nx, nz, ny, NRADM, 0:23)
real(dp) :: EM3RS(nx, nz, ny)

!this array is a split of the daily total emissions into hourly fractions
!em UTC (3h), mas outubro horario verao (2h)
!Ciclo diario emissao CO - base dado Olimpio, experimento Hewllet
!Ciclo diario emissao - medida do tunel 2011
!Perfil de SO2 igual ao de NOx
data hrsplt_co / 0.019_dp, 0.012_dp, 0.008_dp, 0.004_dp, 0.003_dp, 0.003_dp, 0.006_dp, 0.017_dp, &
  0.047_dp, 0.074_dp, 0.072_dp, 0.064_dp, 0.055_dp, 0.052_dp, 0.051_dp, 0.048_dp, 0.052_dp, &
  0.057_dp, 0.068_dp, 0.087_dp, 0.085_dp, 0.057_dp, 0.035_dp, 0.024_dp /

!Ciclo diario emissao NOx
data hrsplt_nox / 0.019_dp, 0.015_dp, 0.012_dp, 0.010_dp, 0.008_dp, 0.009_dp, 0.015_dp, 0.030_dp, &
  0.048_dp, 0.053_dp, 0.051_dp, 0.061_dp, 0.064_dp, 0.064_dp, 0.061_dp, 0.060_dp, 0.060_dp, &
  0.065_dp, 0.065_dp, 0.066_dp, 0.056_dp, 0.044_dp, 0.035_dp, 0.027_dp /

!Esse vetor informa os constituintes a serem salvos no formato WRF/Chem
data ename / 'e_co  ', 'e_co2 ', 'e_ch4 ' /

! Declaração de variáveis do namelist
integer, parameter :: n_tipos = 8, n_substancias = 3

real(dp), dimension(n_tipos) :: intensidades_de_uso, frac_veiculos, emissoes_CO2, emissoes_CO, &
  emissoes_CH4
real(dp) :: fatores_de_emissao_trem(n_substancias), acrescimo_motocicletas

! Namelist para controle das entradas
namelist / entrada / intensidades_de_uso, frac_veiculos, emissoes_CO2, emissoes_CO, emissoes_CH4, &
  fatores_de_emissao_trem, acrescimo_motocicletas

! Necessárias para cálculos
real(dp), dimension(n_tipos) :: emissao_CO2_por_veiculo, emissao_CO_por_veiculo, &
  emissao_CH4_por_veiculo
real(dp) :: acrescimo_porcentagem_motocicletas, porcentagem_restante

! Necessária para verificar erros de leitura do namelist
integer :: arquivo_de_namelist, status_do_namelist

character(len=80) :: linha

! Leitura das entradas via arquivo
open(newunit = arquivo_de_namelist, file = 'namelist.emis', status = 'old', action = 'read')
read(arquivo_de_namelist, nml = entrada, iostat = status_do_namelist)

! Verifica por erro de leitura do namelist
if (status_do_namelist /= 0) then
  backspace arquivo_de_namelist
  read(arquivo_de_namelist, fmt = '(A)') linha

  write(error_unit, '(A//A/A)') 'Falha na leitura do namelist', 'Linha invalida no namelist: ', &
    trim(linha)

  ! Fecha o arquivo de namelist
  close(arquivo_de_namelist)

  call exit(1)
endif

! Fecha o arquivo de namelist
close(arquivo_de_namelist)

acrescimo_porcentagem_motocicletas = acrescimo_motocicletas / 100

porcentagem_restante = 1 - acrescimo_porcentagem_motocicletas

! Aplicando o cenário
frac_veiculos(8) = frac_veiculos(8) + acrescimo_porcentagem_motocicletas
frac_veiculos(1:7) = frac_veiculos(1:7) * porcentagem_restante

emissao_CO_por_veiculo = emissoes_CO * intensidades_de_uso
emissao_CO2_por_veiculo = emissoes_CO2 * intensidades_de_uso
emissao_CH4_por_veiculo = emissoes_CH4 * intensidades_de_uso

!leitura do mapa  (saidaxkm.pgm) com numero de veiculos na celula de x*xkm

open(20, file = 'output_vehicular', status = 'old', action = 'read')                             ! modificar ****************

do j = 1, cy
  read(20, *)(veic(i, j), i = 1, cx)
enddo

close(unit = 20)

!Abrir e ler um arquivo de saida das termeletricas
open(77, file = 'output_refinary', status = 'old', action = 'read')                              ! modificar **************

do j = 1, cy
  read(77, *)(refina(i, j), i = 1, cx)
enddo

close(unit = 77)

open(30, file = 'output_diesel', status = 'old', action = 'read')                                ! modificar *************

do j = 1, cy
  read(30, *)(termo(i, j), i = 1, cx)
enddo

close(unit = 30)

open(40, file = 'output_fueloil', status = 'old', action = 'read')                               ! modificar ************

do j = 1, cy
  read(40, *)(termo2(i, j), i = 1, cx)
enddo

close(unit = 40)

open(50, file = 'output_naturalgas', status = 'old', action = 'read')                            ! modificar ***********

do j = 1, cy
  read(50, *)(termo3(i, j), i = 1, cx)
enddo

close(unit = 50)

!leitura das ferrovias by Reis
!Considera-se para as ferrovias a quantidade de litros
!de combustivel quimados por ponto de grade

open(60, file = 'output_ferrovia', status = 'old', action = 'read')                              ! modificar ***********

do j = 1, cy
  read(60, *)(ferro(i, j), i = 1, cx)
  ferro(i, j) = ferro(i, j) / 24 !dividindo por 24 para conventer l/dia para l/hora
enddo

close(unit = 60)

!multiplicando o numero de veiculos pelo fator de correcao, Jorge 18.2
!e fracao de veiculos correspondente ao tipo de veiculo na RMSP.
veicfc = veic * 18.2_dp

! fracao frota veicular - DENATRAN
veic1 = veicfc * frac_veiculos(1)  ! Gasolina C
veic2 = veicfc * frac_veiculos(2)  ! Alcool
veic3 = veicfc * frac_veiculos(3)  ! Flex
veic4a = veicfc * frac_veiculos(4)  ! Caminhoes
veic4b = veicfc * frac_veiculos(5) ! Urbanos
veic4c = veicfc * frac_veiculos(6)  ! Rodoviarios
veic5 = veicfc * frac_veiculos(7)  ! Taxis
veic6 = veicfc * frac_veiculos(8)  ! Motocicletas

! Multiplicando pelo fator de emissao, e considerando que os leves rodam
! por dia 46.6 km (hewlett) e os pesados 100km. (Foi alterado!)
! Aterada a quilometragem baseada nas informacoes contidas em:
! http://www.sptrans.com.br/ganhosambientais/ (site da SPTrans)
! A nova quilometragem media por dia fica:
! veiculos a gasolina rodam: 41,09 km/dia
! veiculos a alcool+flex rodam: 41,09 km/dia
! veiculos a diesel (caminhoes) rodam: 109,58 km/dia
! veiculos a diesel (onibus-EURO II) rodam: 164,38 km/dia
! veiculos a diesel (onibus-EURO III) rodam: 164,38 km/dia
! veiculos a gas natural rodam: 41,09 km/dia
! veiculos motocicletas rodam: 136,98 km/dia
! Fatores medios de emissao CETESB,2008 referentes ao ano de 2007, e SPTrans.
! O fator relativo a concentracao de VOC foi baseado no HC. O antigo era:
! VOC leves = 3.12 g/km , VOC pesados = 2.29 g/km
! FE experimento tuneles + Cetesb, 2011

!Fe co2 = 600.49 os dados da CETESB são proximos
!Fe ch4 = 0.6 dado CTESB 2020

veic_ch4 = veic1 * emissao_CH4_por_veiculo(1) + &
  veic2 * emissao_CH4_por_veiculo(2) + &
  veic3 * emissao_CH4_por_veiculo(3) + &
  veic4a * emissao_CH4_por_veiculo(4) + &
  veic4b * emissao_CH4_por_veiculo(5) + &
  veic4c * emissao_CH4_por_veiculo(6) + &    !conferir
  veic5 * emissao_CH4_por_veiculo(7) + &
  veic6 * emissao_CH4_por_veiculo(8)

veic_co2 = veic1 * emissao_CO2_por_veiculo(1) + &
  veic2 * emissao_CO2_por_veiculo(2) + &
  veic3 * emissao_CO2_por_veiculo(3) + &
  veic4a * emissao_CO2_por_veiculo(4) + &
  veic4b * emissao_CO2_por_veiculo(5) + &
  veic4c * emissao_CO2_por_veiculo(6) + &    !conferir
  veic5 * emissao_CO2_por_veiculo(7) + &
  veic6 * emissao_CO2_por_veiculo(8)

veic_co = veic1 * emissao_CO_por_veiculo(1) + &
  veic2 * emissao_CO_por_veiculo(2) + &
  veic3 * emissao_CO_por_veiculo(3) + &
  veic4a * emissao_CO_por_veiculo(4) + &
  veic4b * emissao_CO_por_veiculo(5) + &
  veic4c * emissao_CO_por_veiculo(6) + &
  veic5 * emissao_CO_por_veiculo(7) + &
  veic6 * emissao_CO_por_veiculo(8)

veic_ch4 = veic_ch4 / 16                 !    mol/dia
veic_co2 = veic_co2 / 44                 !    mol/dia
veic_co = veic_co / 28                  !    mol/dia

!Adicionando emissao dos trens by Reis
trem_ch4 = ferro * fatores_de_emissao_trem(3) / 16
trem_co2 = ferro * fatores_de_emissao_trem(2) / 44
trem_co = ferro * fatores_de_emissao_trem(1) / 28

!Aplica o perfil diurno
do j = 1, cx
  perfis_diurnos_veic_ch4(0:23, :, 1:) = veic_ch4(j, :) * hrsplt_co !    mol/km^2/hr
  perfis_diurnos_veic_co2(0:23, :, 1:) = veic_co2(j, :) * hrsplt_co(0:23) !    mol/km^2/hr
  perfis_diurnos_veic_co(0:23, :, 1:) = veic_co(j, :) * hrsplt_co(0:23) !    mol/km^2/hr
enddo

do ihr = 0, 23
  !Aplica o perfil diurno
  ! perfil_diurno_veic_ch4 = veic_ch4 * hrsplt_co(ihr)     !    mol/km^2/hr
  ! perfil_diurno_veic_co2 = veic_co2 * hrsplt_co(ihr)     !    mol/km^2/hr
  ! perfil_diurno_veic_co = veic_co * hrsplt_co(ihr)      !    mol/km^2/hr

  ! Calculando em apenas dois niveis verticais!
  do k = 1, 2
    e_ch4(:, :, k, ihr) = perfis_diurnos_veic_ch4(ihr) + trem_ch4
    e_co2(:, :, k, ihr) = perfis_diurnos_veic_co2(ihr) + trem_co2
    e_co(:, :, k, ihr) = perfis_diurnos_veic_co(ihr) + termo_co + termo2_co + termo3_co + &
      refinaria_co + trem_co

    EM3RD(:, k, :, 32, ihr) = e_ch4(:, :, k, ihr) ! Add by REIS
    EM3RD(:, k, :, 31, ihr) = e_co2(:, :, k, ihr) ! Add by REIS
    EM3RD(:, k, :, 11, ihr) = e_co(:, :, k, ihr)
  enddo
enddo

!******************escrevendo os resultados**********
open(50, file = 'wrfem_00to12z_d01', form = 'formatted', action = 'write')                 ! modificar

do cont = 1, 2
  if (cont == 1) then
    print *, 'Salvando : wrfem_00to12z_d01'

    cont1 = 50   !Arquivo a ser aberto
    cont2 = 0   !hora inicial
    cont3 = 11  !hora final
  else
    print *, 'Fechou: wrfem_00to12z_d01'
    print *, 'Salvando : wrfem_12to24z_d01'

    close(unit = 50)

    open(7, file = 'wrfem_12to24z_d01', form = 'formatted', action = 'write')              ! modificar

    cont1 = 7  !arquivo a ser aberto
    cont2 = 12 !hora inicial
    cont3 = 23 !hora final
  endif

  write(cont1, *) NRADM !Escreve a qtd de poluentes
  write(cont1, *) ename !escreve o nome dos poluentes

  do ihr = cont2, cont3

  write(cont1, *) ihr !escreve a hora

  do concurrent (n = 1:NRADM)
  do j = 1, ny

  ! Escrevendo...
  EM3RS(:, :, j) = EM3RD(:, :, cy - j, n, ihr) ! esta escrevendo invertido
  ! variacao horaria  das emissoes

  enddo

  write(cont1, *) EM3RS

  enddo   !Poluentes
  enddo   !hora
enddo   !Arquivos

! formatos de saida dos dados
! 200    format(5(F20.3, 1X))
! 400    format(A9, I3, E10.3)

close(unit = 7)
close(unit = 5)

! end of program
print *, '******** Fim do Programa *********'

endprogram gera
