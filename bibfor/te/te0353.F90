subroutine te0353(option, nomte)
! ======================================================================
! COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
! THIS PROGRAM IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR MODIFY
! IT UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE AS PUBLISHED BY
! THE FREE SOFTWARE FOUNDATION; EITHER VERSION 2 OF THE LICENSE, OR
! (AT YOUR OPTION) ANY LATER VERSION.
!
! THIS PROGRAM IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL, BUT
! WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF
! MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. SEE THE GNU
! GENERAL PUBLIC LICENSE FOR MORE DETAILS.
!
! YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE
! ALONG WITH THIS PROGRAM; IF NOT, WRITE TO EDF R&D CODE_ASTER,
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
    implicit none
#include "jeveux.h"
#include "asterfort/dfdm2d.h"
#include "asterfort/elref4.h"
#include "asterfort/jevech.h"
#include "asterfort/lteatt.h"
#include "asterfort/rcfonc.h"
#include "asterfort/rctrac.h"
#include "asterfort/rctype.h"
#include "asterfort/rcvalb.h"
#include "asterfort/rcvarc.h"
#include "asterfort/tecach.h"
#include "asterfort/utmess.h"
!
    character(len=16) :: option, nomte
! ......................................................................
!    - FONCTION REALISEE:  CALCUL DES VECTEURS ELEMENTAIRES
!                          OPTION : 'CHAR_MECA_META_Z  '
!    - ARGUMENTS:
!        DONNEES:      OPTION       -->  OPTION DE CALCUL
!                      NOMTE        -->  NOM DU TYPE ELEMENT
! ......................................................................
!-----------------------------------------------------------------------
    integer :: icompo, icontr, iret1, ivari, j, k, lgpg
    integer :: mater, nbcon, nbres
    real(kind=8) :: sigmo
!-----------------------------------------------------------------------
    parameter(nbres=21)
    character(len=8) :: nomres(nbres), nomcle(5), acier(4), zirc(2), type
    character(len=4) :: fami
    integer :: icodre(nbres)
    integer :: test
    character(len=16) :: compor
    real(kind=8) :: valres(nbres), e, nu
    real(kind=8) :: zfbm, sig(4), kpt(5), sigdv(6), deuxmu, zalpha
    real(kind=8) :: dfdx(9), dfdy(9), tpg, poids, r, co, kron(6)
    real(kind=8) :: r0(5), rprim, vi(5), r8bid
    real(kind=8) :: phas(5), coef, zvarim, zvarip, deltaz, trans
    real(kind=8) :: phasp(4), phasm(4), resu
    integer :: nno, kp, npg1, i, ivectu, jtab(7), l, iret
    integer :: ipoids, ivf, idfde, igeom, imate
    integer :: jprol, jvale, nbval, ndim, nnos, jgano
    logical :: laxi
    data kron/1.d0,1.d0,1.d0,0.d0,0.d0,0.d0/
    data acier/'PFERRITE','PPERLITE','PBAINITE','PMARTENS'/
    data zirc/'ALPHPUR','ALPHBETA'/
!
    fami='RIGI'
    call elref4(' ', fami, ndim, nno, nnos,&
                npg1, ipoids, ivf, idfde, jgano)
    nbcon=4
    laxi=.false.
    if (lteatt('AXIS','OUI')) laxi=.true.
    call jevech('PGEOMER', 'L', igeom)
    call jevech('PMATERC', 'L', imate)
    mater=zi(imate)
    call jevech('PCONTMR', 'L', icontr)
    call jevech('PCOMPOR', 'L', icompo)
    compor=zk16(icompo+7)
    call tecach('OON', 'PVARIPR', 'L', iret, nval=7,&
                itab=jtab)
    lgpg=max(jtab(6),1)*jtab(7)
    call jevech('PVARIPR', 'L', ivari)
    call jevech('PVECTUR', 'E', ivectu)
    if (compor(1:5) .eq. 'ACIER') then
        nomres(1)='E'
        nomres(2)='NU'
        nomres(3)='F1_K'
        nomres(4)='F2_K'
        nomres(5)='F3_K'
        nomres(6)='F4_K'
        nomres(7)='F1_D_F_M'
        nomres(8)='F2_D_F_M'
        nomres(9)='F3_D_F_M'
        nomres(10)='F4_D_F_M'
        nomres(11)='SY_MELAN'
        if (zk16(icompo)(1:9) .eq. 'META_P_IL' .or. zk16(icompo)(1:9) .eq. 'META_V_IL' .or.&
            zk16(icompo)(1:9) .eq. 'META_P_CL' .or. zk16(icompo)(1:9) .eq. 'META_V_CL') then
            nomres(12)='F1_D_SIGM'
            nomres(13)='F2_D_SIGM'
            nomres(14)='F3_D_SIGM'
            nomres(15)='F4_D_SIGM'
            nomres(16)='C_D_SIGM'
        endif
        if (zk16(icompo)(1:10) .eq. 'META_P_INL' .or. zk16(icompo)(1:10) .eq. 'META_V_INL') then
            nomcle(1)='SIGM_F1'
            nomcle(2)='SIGM_F2'
            nomcle(3)='SIGM_F3'
            nomcle(4)='SIGM_F4'
            nomcle(5)='SIGM_C'
        endif
        nomres(17)='F1_ETA'
        nomres(18)='F2_ETA'
        nomres(19)='F3_ETA'
        nomres(20)='F4_ETA'
        nomres(21)='C_ETA'
    else if (compor(1:4).eq.'ZIRC') then
        nomres(1)='E'
        nomres(2)='NU'
        nomres(3)='F1_K'
        nomres(4)='F2_K'
        nomres(5)='F1_D_F1_ME'
        nomres(6)='F2_D_F1_ME'
        nomres(7)='SY_MELAN'
        if (zk16(icompo)(1:9) .eq. 'META_P_IL' .or. zk16(icompo)(1:9) .eq. 'META_V_IL') then
            nomres(8)='F1_D_SIGM'
            nomres(9)='F2_D_SIGM'
            nomres(10)='C_D_SIGM'
        endif
        if (zk16(icompo)(1:10) .eq. 'META_P_INL' .or. zk16(icompo)(1:10) .eq. 'META_V_INL') then
            nomcle(1)='SIGM_F1'
            nomcle(2)='SIGM_F2'
            nomcle(3)='SIGM_C'
        endif
        nomres(14)='F1_ETA'
        nomres(15)='F2_ETA'
        nomres(16)='C_ETA'
    endif
    do 10 i = 1, nbres
10  end do
    do 160 kp = 1, npg1
        k=(kp-1)*nno
        call dfdm2d(nno, kp, ipoids, idfde, zr(igeom),&
                    poids, dfdx, dfdy)
        r=0.d0
        call rcvarc(' ', 'TEMP', '+', 'RIGI', kp,&
                    1, tpg, iret1)
!
        if (compor(1:5) .eq. 'ACIER') then
            do 20 l = 1, 4
                call rcvarc(' ', acier(l), '-', 'RIGI', kp,&
                            1, phasm(l), iret)
                if (iret .eq. 1) phasm(l)=0.d0
                call rcvarc(' ', acier(l), '+', 'RIGI', kp,&
                            1, phasp(l), iret)
                if (iret .eq. 1) phasp(l)=0.d0
20          continue
        else if (compor(1:4).eq.'ZIRC') then
            do 30 l = 1, 2
                call rcvarc(' ', zirc(l), '-', 'RIGI', kp,&
                            1, phasm(l), iret)
                if (iret .eq. 1) phasm(l)=0.d0
                call rcvarc(' ', zirc(l), '+', 'RIGI', kp,&
                            1, phasp(l), iret)
                if (iret .eq. 1) phasp(l)=0.d0
30          continue
        endif
        do 40 i = 1, nno
            r=r+zr(igeom+2*(i-1))*zr(ivf+k+i-1)
40      continue
        call rcvalb(fami, kp, 1, '+', mater,&
                    ' ', 'ELAS_META', 0, ' ', [0.d0],&
                    2, nomres, valres, icodre, 1)
        e=valres(1)
        nu=valres(2)
        deuxmu=e/(1.d0+nu)
!
        if (compor(1:5) .eq. 'ACIER') then
            call rcvalb(fami, kp, 1, '+', mater,&
                        ' ', 'META_PT', 0, ' ', [0.d0],&
                        4, nomres(3), valres(3), icodre(3), 0)
            do 50 i = 3, 6
                if (icodre(i) .ne. 0) then
                    kpt(i-2)=0.d0
                else
                    kpt(i-2)=valres(i)
                endif
50          continue
            zfbm=phasp(1)
            do 60 i = 1, 3
                zfbm=zfbm+phasp(1+i)
60          continue
            trans=0.d0
            do 70 i = 1, 4
                zvarim=phasm(i)
                zvarip=phasp(i)
                deltaz=(zvarip-zvarim)
                if (deltaz .gt. 0) then
                    j=6+i
                    call rcvalb('RIGI', 1, 1, '+', mater,&
                                ' ', 'META_PT', 1, 'META', [zfbm],&
                                1, nomres(j), valres(j), icodre(j), 0)
                    if (icodre(j) .ne. 0) valres(j)=0.d0
                    trans=trans+kpt(i)*valres(j)*deltaz
                endif
70          continue
            call rcvalb(fami, kp, 1, '+', mater,&
                        ' ', 'META_VISC', 0, ' ', [0.d0],&
                        5, nomres(17), valres(17), icodre(17), 0)
            test=1
            do 80 i = 17, 21
                if (icodre(i) .eq. 0) test=0
80          continue
!
!
            if (zk16(icompo)(1:8) .eq. 'META_P_I' .or. zk16(icompo)(1:8) .eq. 'META_V_I') then
                if ((zr(ivari+(kp-1)*lgpg+5).gt.0.5d0) .and. ( test.eq.1)) then
                    call rcvalb('RIGI', 1, 1, '+', mater,&
                                ' ', 'ELAS_META', 1, 'META', [zfbm],&
                                1, nomres(11), valres(11), icodre(11), 0)
                    if (icodre(11) .ne. 0) then
                        valres(11)=zfbm
                    endif
!
                    phas(1)=phasp(1)
                    phas(2)=phasp(2)
                    phas(3)=phasp(3)
                    phas(4)=phasp(4)
                    phas(5)=1.d0-(phas(1)+phas(2)+phas(3)+phas(4))
                    if (zk16(icompo)(1:9) .eq. 'META_P_IL' .or. zk16( icompo)(1:9) .eq.&
                        'META_V_IL') then
!
                        call rcvalb(fami, kp, 1, '+', mater,&
                                    ' ', 'META_ECRO_LINE', 0, ' ', [0.d0],&
                                    5, nomres(12), valres(12), icodre(12), 1)
                        r0(1)=valres(12)*e/(e-valres(12))
                        r0(2)=valres(13)*e/(e-valres(13))
                        r0(3)=valres(14)*e/(e-valres(14))
                        r0(4)=valres(15)*e/(e-valres(15))
                        r0(5)=valres(16)*e/(e-valres(16))
                    endif
                    if (zk16(icompo)(1:9) .eq. 'META_P_CL' .or. zk16( icompo)(1:9) .eq.&
                        'META_V_CL') then
                        call rcvalb('RIGI', 1, 1, '+', mater,&
                                    ' ', 'META_ECRO_LINE', 1, 'TEMP', [tpg],&
                                    5, nomres(12), valres(12), icodre(12), 1)
                        r0(1)=(2.d0/3.d0)*valres(12)*e/(e-valres(12))
                        r0(2)=(2.d0/3.d0)*valres(13)*e/(e-valres(13))
                        r0(3)=(2.d0/3.d0)*valres(14)*e/(e-valres(14))
                        r0(4)=(2.d0/3.d0)*valres(15)*e/(e-valres(15))
                        r0(5)=(2.d0/3.d0)*valres(16)*e/(e-valres(16))
                    endif
!
                    if (zk16(icompo)(1:10) .eq. 'META_P_INL' .or. zk16(icompo)(1:10) .eq.&
                        'META_V_INL') then
                        vi(1)=zr(ivari+(kp-1)*lgpg)
                        vi(2)=zr(ivari+(kp-1)*lgpg+1)
                        vi(3)=zr(ivari+(kp-1)*lgpg+2)
                        vi(4)=zr(ivari+(kp-1)*lgpg+3)
                        vi(5)=zr(ivari+(kp-1)*lgpg+4)
                        do 90 i = 1, 5
                            call rctype(mater, 1, 'TEMP', [tpg], resu,&
                                        type)
                            if ((type.eq.'TEMP') .and. (iret1.eq.1)) then
                                call utmess('F', 'CALCULEL_31')
                            endif
                            call rctrac(mater, 2, nomcle(i), resu, jprol,&
                                        jvale, nbval, r8bid)
                            call rcfonc('V', 2, jprol, jvale, nbval,&
                                        r8bid, r8bid, r8bid, vi(i), r8bid,&
                                        r0(i), r8bid, r8bid, r8bid)
90                      continue
                    endif
                    if (zfbm .gt. 0.d0) then
                        rprim=phas(1)*r0(1)+phas(2)*r0(2)+phas(3)*r0(&
                        3)+ phas(4)*r0(4)
                        rprim=rprim/zfbm
                    else
                        rprim=0.d0
                    endif
                    rprim=(1.d0-valres(11))*r0(5)+valres(11)*rprim
                    coef=1.d0-(1.5d0*deuxmu)/(1.5d0*deuxmu+rprim)
                else
                    coef=1.d0
                endif
                elseif (zk16(icompo)(1:8).eq.'META_P_C' .or. zk16(icompo)(&
            1:8).eq.'META_V_C') then
                if ((zr(ivari+(kp-1)*lgpg+36).gt.0.5d0) .and. ( test.eq.1)) then
                    call rcvalb('RIGI', 1, 1, '+', mater,&
                                ' ', 'ELAS_META', 1, 'META', [zfbm],&
                                1, nomres(11), valres(11), icodre(11), 0)
                    if (icodre(11) .ne. 0) then
                        valres(11)=zfbm
                    endif
!
                    phas(1)=phasp(1)
                    phas(2)=phasp(2)
                    phas(3)=phasp(3)
                    phas(4)=phasp(4)
                    phas(5)=1.d0-(phas(1)+phas(2)+phas(3)+phas(4))
                    call rcvalb(fami, kp, 1, '+', mater,&
                                ' ', 'META_ECRO_LINE', 0, ' ', [0.d0],&
                                5, nomres(12), valres( 12), icodre(12), 1)
                    r0(1)=(2.d0/3.d0)*valres(12)*e/(e-valres(12))
                    r0(2)=(2.d0/3.d0)*valres(13)*e/(e-valres(13))
                    r0(3)=(2.d0/3.d0)*valres(14)*e/(e-valres(14))
                    r0(4)=(2.d0/3.d0)*valres(15)*e/(e-valres(15))
                    r0(5)=(2.d0/3.d0)*valres(16)*e/(e-valres(16))
                    if (zfbm .gt. 0.d0) then
                        rprim=phas(1)*r0(1)+phas(2)*r0(2)+phas(3)*r0(&
                        3)+ phas(4)*r0(4)
                        rprim=rprim/zfbm
                    else
                        rprim=0.d0
                    endif
                    rprim=(1.d0-valres(11))*r0(5)+valres(11)*rprim
                    coef=1.d0-(1.5d0*deuxmu)/(1.5d0*deuxmu+rprim)
                else
                    coef=1.d0
                endif
            endif
!
!
!
        else if (compor(1:4).eq.'ZIRC') then
            call rcvalb(fami, kp, 1, '+', mater,&
                        ' ', 'META_PT', 0, ' ', [0.d0],&
                        2, nomres(3), valres(3), icodre(3), 0)
            do 100 i = 3, 4
                if (icodre(i) .ne. 0) then
                    kpt(i-2)=0.d0
                else
                    kpt(i-2)=valres(i)
                endif
100          continue
!
            zalpha=phasp(1)+phasp(2)
            zvarim=phasm(1)
            zvarip=phasp(1)
            deltaz=(zvarip-zvarim)
            trans=0.d0
            if (deltaz .gt. 0) then
                call rcvalb('RIGI', 1, 1, '+', mater,&
                            ' ', 'META_PT', 1, 'META', [zfbm],&
                            1, nomres(5), valres(5), icodre(5), 0)
                if (icodre(5) .ne. 0) valres(5)=0.d0
                trans=trans+kpt(1)*valres(5)*deltaz
            endif
            zvarim=phasm(2)
            zvarip=phasp(2)
            deltaz=(zvarip-zvarim)
            if (deltaz .gt. 0) then
                call rcvalb('RIGI', 1, 1, '+', mater,&
                            ' ', 'META_PT', 1, 'META', [zfbm],&
                            1, nomres(6), valres(6), icodre(6), 0)
                if (icodre(6) .ne. 0) valres(6)=0.d0
                trans=trans+kpt(2)*valres(6)*deltaz
            endif
            call rcvalb(fami, kp, 1, '+', mater,&
                        ' ', 'META_VISC', 0, ' ', [0.d0],&
                        3, nomres(14), valres(14), icodre(14), 0)
            test=1
            do 110 i = 14, 16
                if (icodre(14) .eq. 0) test=0
110          continue
            if ((zr(ivari+(kp-1)*lgpg+3).gt.0.5d0) .and. (test.eq.1)) then
                call rcvalb('RIGI', 1, 1, '+', mater,&
                            ' ', 'ELAS_META', 1, 'META', [zalpha],&
                            1, nomres(7), valres(7), icodre(7), 0)
                if (icodre(7) .ne. 0) then
                    valres(7)=zalpha
                endif
!
                phas(1)=phasp(1)
                phas(2)=phasp(2)
                phas(3)=1.d0-(phas(1)+phas(2))
                if (zk16(icompo)(1:9) .eq. 'META_P_IL' .or. zk16(icompo) (1:9) .eq.&
                    'META_V_IL') then
                    call rcvalb(fami, kp, 1, '+', mater,&
                                ' ', 'META_ECRO_LINE', 0, ' ', [0.d0],&
                                3, nomres(8), valres(8), icodre(8), 1)
                    r0(1)=valres(8)*e/(e-valres(8))
                    r0(2)=valres(9)*e/(e-valres(9))
                    r0(3)=valres(10)*e/(e-valres(10))
                endif
                if (zk16(icompo)(1:9) .eq. 'META_P_CL' .or. zk16(icompo) (1:9) .eq.&
                    'META_V_CL') then
                    call rcvalb(fami, kp, 1, '+', mater,&
                                ' ', 'META_ECRO_LINE', 0, ' ', [0.d0],&
                                5, nomres(8), valres(8), icodre(8), 1)
                    r0(1)=(2.d0/3.d0)*valres(8)*e/(e-valres(8))
                    r0(2)=(2.d0/3.d0)*valres(9)*e/(e-valres(9))
                    r0(3)=(2.d0/3.d0)*valres(10)*e/(e-valres(10))
!
                endif
!
!
                if (zk16(icompo)(1:10) .eq. 'META_P_INL' .or. zk16( icompo)(1:10) .eq.&
                    'META_V_INL') then
                    vi(1)=zr(ivari+(kp-1)*lgpg)
                    vi(2)=zr(ivari+(kp-1)*lgpg+1)
                    vi(3)=zr(ivari+(kp-1)*lgpg+2)
                    do 120 i = 1, 3
                        call rctype(mater, 1, 'TEMP', [tpg], resu,&
                                    type)
                        if ((type.eq.'TEMP') .and. (iret1.eq.1)) then
                            call utmess('F', 'CALCULEL_31')
                        endif
                        call rctrac(mater, 3, nomcle(i), tpg, jprol,&
                                    jvale, nbval, r8bid)
                        call rcfonc('V', 3, jprol, jvale, nbval,&
                                    r8bid, r8bid, r8bid, vi(i), r8bid,&
                                    r0(i), r8bid, r8bid, r8bid)
120                  continue
                endif
                if (zalpha .gt. 0.d0) then
                    rprim=phas(1)*r0(1)+phas(2)*r0(2)
                    rprim=rprim/zalpha
                else
                    rprim=0.d0
                endif
                rprim=(1.d0-valres(7))*r0(3)+valres(7)*rprim
                coef=1.d0-(1.5d0*deuxmu)/(1.5d0*deuxmu+rprim)
            else
                coef=1.d0
            endif
        endif
        if (laxi) then
            poids=poids*r
            co=1.d0/r
        else
            co=0.d0
        endif
        sigmo=0.d0
        do 130 i = 1, 3
            sigmo=sigmo+zr(icontr+(kp-1)*nbcon+i-1)
130      continue
        sigmo=sigmo/3.d0
        do 140 i = 1, nbcon
            sigdv(i)=zr(icontr+(kp-1)*nbcon+i-1)-sigmo*kron(i)
            sig(i)=coef*(1.5d0*trans*sigdv(i))
            sig(i)=deuxmu*sig(i)
140      continue
        do 150 i = 1, nno
            zr(ivectu+2*i-2)=zr(ivectu+2*i-2)+ poids*(sig(1)*dfdx(i)+&
            sig(3)*zr(ivf+k+i-1)* co+sig(4)*dfdy(i))
            zr(ivectu+2*i-1)=zr(ivectu+2*i-1)+ poids*(sig(2)*dfdy(i)+&
            sig(4)*dfdx(i))
150      continue
160  end do
end subroutine
