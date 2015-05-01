subroutine dglrda()
! aslint: disable=W1501
! ======================================================================
! COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: sebastien.fayolle at edf.fr
    implicit none
#include "jeveux.h"
#include "asterc/getfac.h"
#include "asterc/getres.h"
#include "asterfort/codent.h"
#include "asterfort/foimpr.h"
#include "asterfort/fointe.h"
#include "asterfort/gcncon.h"
#include "asterfort/getvid.h"
#include "asterfort/getvr8.h"
#include "asterfort/infmaj.h"
#include "asterfort/infniv.h"
#include "asterfort/interf.h"
#include "asterfort/jedema.h"
#include "asterfort/jeecra.h"
#include "asterfort/jemarq.h"
#include "asterfort/mmfonc.h"
#include "asterfort/mocon2.h"
#include "asterfort/moconm.h"
#include "asterfort/parglr.h"
#include "asterfort/rcvale.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
!
! ----------------------------------------------------------------------
!
! BUT : DETERMINATION DES PARAMETRES MATERIAU POUR LE MODELE GLRC_DAMAGE
!
!-----------------------------------------------------------------------
!
    integer :: na
    parameter (na=10)
    integer :: nnap, nprec, nliner, ibid, ii, ilit, nlit, jlm, jmelk
    integer :: jmelr, jmelc, lonobj, ifon0, longf, ifon1, lonuti
    real(kind=8) :: eb, nub, ft, fc, gamma, num, em
    integer :: niv, ifm
    real(kind=8) :: qp1, qp2, prex, prey, nmin0
    real(kind=8), dimension(2, 3) :: cn, cm
    real(kind=8) :: ea(3*na), omx(3*na), omy(3*na), sy(3*na), nua(3*na)
    real(kind=8) :: rx(3*na), ry(3*na), rlr(na), liner(3*na), bn11, bn12, bn22
    real(kind=8) :: bn33
    real(kind=8) :: hh, bm11, bm12, bm22, bm33, bc11, bc22, mf1x, mf1y
    real(kind=8) :: mf2x, mf2y, rho, amora, amorb, amorh, eeq, nueq, mf1, mf2
    real(kind=8) :: normm, normn, valres(5), mp1n0, mp2n0, aux, maxmp(2)
    real(kind=8) :: minmp(2), alphat
    real(kind=8) :: nmax0, nmin(2), nmax(2), oml(na), r8b(1), par1, par2, elb(2)
    real(kind=8) :: mp1cst(2), mp2cst(2), omt, eat, bt1, bt2, pflua, pretr
    integer :: icodr2(5), nalphat
    character(len=6) :: k6
    character(len=8) :: mater, fon(4), k8b
    character(len=16) :: nomres(5)
    character(len=8) :: fsncx, fsncy, fscxd, fscyd, fscxd2, fscyd2
    character(len=8) :: fincx, fincy, ficxd, ficyd, ficxd2, ficyd2
    character(len=16) :: type, nomcmd
    integer :: impf, icst, icis
!
    call jemarq()
!
    call getfac('NAPPE', nnap)
    call getfac('CABLE_PREC', nprec)
    call getfac('LINER', nliner)
!
    nlit = nnap + nprec + nliner
!
    if (nlit .eq. 0) then
        nlit = 1
        ea(1)  = 0.0d0
        nua(1) = 0.0d0
        omx(1) = 0.0d0
        omy(1) = 0.0d0
        rx(1)  = 0.0d0
        ry(1)  = 0.0d0
    endif
!
! --- RECUPERATION DU NIVEAU D'IMPRESSION
    call infmaj()
    call infniv(ifm, niv)
!
    call getvid('BETON', 'MATER', iocc=1, scal=mater, nbret=ibid)
!
    nomres(1) = 'E'
    nomres(2) = 'NU'
    nomres(3) = 'RHO'
!
    k8b = ' '
    r8b(1) = 0.d0
    call rcvale(mater, 'ELAS            ', 0, k8b, r8b(1), 3, nomres, valres, icodr2, 1)
!
    eb = valres(1)
    nub = valres(2)
    rho = valres(3)
!
    nomres(1) = 'AMOR_ALPHA'
    nomres(2) = 'AMOR_BETA'
    nomres(3) = 'AMOR_HYST'
!
    call rcvale(mater, 'ELAS            ', 0, k8b, r8b(1), 2, nomres, valres, icodr2, 0)
!
    if (icodr2(1) .ne. 0) then
        amora = 0.0d0
    else
        amora = valres(1)
    endif
!
    if (icodr2(2) .ne. 0) then
        amorb = 0.0d0
    else
        amorb = valres(2)
    endif
!
    if (icodr2(3) .ne. 0) then
        amorh = 0.0d0
    else
        amorh = valres(3)
    endif
!
    nomres(1) = 'SYT'
    nomres(2) = 'SYC'
    call rcvale(mater, 'BETON_ECRO_LINE ', 0, k8b, r8b(1), 2, nomres, valres, icodr2, 1)
    ft = valres(1)
    fc = valres(2)
!
! PERT_FLUA et PERT_RETR
    nomres(1) = 'PERT_FLUA'
    nomres(2) = 'PERT_RETR'
    call rcvale(mater, 'BPEL_BETON      ', 0, k8b, r8b(1), 2, nomres, valres, icodr2, 0)
    if (icodr2(1) .eq. 0) then
        pflua = valres(1)
    else
        pflua = 0.d0
    endif
!
    if (icodr2(2) .eq. 0) then
        pretr = valres(2)
    else
        pretr = 0.d0
    endif
!
    call getvr8('BETON', 'EPAIS', iocc=1, scal=hh, nbret=ibid)
    call getvr8('BETON', 'GAMMA', iocc=1, scal=gamma, nbret=ibid)
    call getvr8('BETON', 'QP1', iocc=1, scal=qp1, nbret=ibid)
    call getvr8('BETON', 'QP2', iocc=1, scal=qp2, nbret=ibid)
    call getvr8('BETON', 'C1N1', iocc=1, scal=cn(1, 1), nbret=ibid)
    call getvr8('BETON', 'C1N2', iocc=1, scal=cn(1, 2), nbret=ibid)
    call getvr8('BETON', 'C1N3', iocc=1, scal=cn(1, 3), nbret=ibid)
    call getvr8('BETON', 'C2N1', iocc=1, scal=cn(2, 1), nbret=ibid)
    call getvr8('BETON', 'C2N2', iocc=1, scal=cn(2, 2), nbret=ibid)
    call getvr8('BETON', 'C2N3', iocc=1, scal=cn(2, 3), nbret=ibid)
    call getvr8('BETON', 'C1M1', iocc=1, scal=cm(1, 1), nbret=ibid)
    call getvr8('BETON', 'C1M2', iocc=1, scal=cm(1, 2), nbret=ibid)
    call getvr8('BETON', 'C1M3', iocc=1, scal=cm(1, 3), nbret=ibid)
    call getvr8('BETON', 'C2M1', iocc=1, scal=cm(2, 1), nbret=ibid)
    call getvr8('BETON', 'C2M2', iocc=1, scal=cm(2, 2), nbret=ibid)
    call getvr8('BETON', 'C2M3', iocc=1, scal=cm(2, 3), nbret=ibid)
!
    if (nnap .gt. 0) then
        do ilit = 1, nnap
            call getvid('NAPPE', 'MATER', iocc=ilit, scal=mater, nbret=ibid)
            nomres(1) = 'E'
            call rcvale(mater, 'ELAS            ', 0, k8b, r8b(1), 1, nomres, valres, icodr2, 1)
            ea(ilit) = valres(1)
            nomres(1) = 'SY'
            call rcvale(mater, 'ECRO_LINE       ', 0, k8b, r8b(1), 1, nomres, valres, icodr2, 1)
            sy(ilit) = valres(1)
            call getvr8('NAPPE', 'OMX', iocc=ilit, scal=omx(ilit), nbret=ibid)
            call getvr8('NAPPE', 'OMY', iocc=ilit, scal=omy(ilit), nbret=ibid)
            call getvr8('NAPPE', 'RX', iocc=ilit, scal=rx(ilit), nbret=ibid)
            call getvr8('NAPPE', 'RY', iocc=ilit, scal=ry(ilit), nbret=ibid)
            nua(ilit) = 0.0d0
            liner(ilit) = 0.0d0
        end do
        ilit = nnap
    endif
!
    if (nprec .gt. 0) then
        do ii = 1, nprec
            ilit = ilit + 1
            call getvid('CABLE_PREC', 'MATER', iocc=ii, scal=mater, nbret=ibid)
            nomres(1) = 'E'
            call rcvale(mater, 'ELAS            ', 0, k8b, r8b(1), 1, nomres, valres, icodr2, 1)
            ea(ilit) = valres(1)
            nomres(1) = 'SY'
            call rcvale(mater, 'ECRO_LINE       ', 0, k8b, r8b(1), 1, nomres, valres, icodr2, 1)
            sy(ilit) = valres(1)
            call getvr8('CABLE_PREC', 'OMX', iocc=ii, scal=omx(ilit), nbret=ibid)
            call getvr8('CABLE_PREC', 'OMY', iocc=ii, scal=omy(ilit), nbret=ibid)
            call getvr8('CABLE_PREC', 'RX', iocc=ii, scal=rx(ilit), nbret=ibid)
            call getvr8('CABLE_PREC', 'RY', iocc=ii, scal=ry(ilit), nbret=ibid)
            call getvr8('CABLE_PREC', 'PREX', iocc=ii, scal=prex, nbret=ibid)
            call getvr8('CABLE_PREC', 'PREY', iocc=ii, scal=prey, nbret=ibid)
            nua(ilit) = 0.0d0
            liner(ilit) = 0.0d0
        end do
    else
        prex = 0.0d0
        prey = 0.0d0
    endif
!
    if (nliner .gt. 0) then
        do ii = 1, nliner
            ilit = ilit + 1
            call getvid('LINER', 'MATER', iocc=ii, scal=mater, nbret=ibid)
            nomres(1) = 'E'
            nomres(2) = 'NU'
            call rcvale(mater, 'ELAS            ', 0, k8b, r8b(1), 2, nomres, valres, icodr2, 1)
            ea(ilit) = valres(1)
            nua(ilit) = valres(2)
            nomres(1) = 'SY'
            call rcvale(mater, 'ECRO_LINE       ', 0, k8b, r8b(1), 1, nomres, valres, icodr2, 1)
            sy(ilit) = valres(1)
            call getvr8('LINER', 'OML', iocc=ii, scal=oml(ii), nbret=ibid)
            call getvr8('LINER', 'RLR', iocc=ii, scal=rlr(ii), nbret=ibid)
            rx(ilit) = rlr(ii)
            ry(ilit) = rlr(ii)
            omx(ilit) = oml(ii)
            omy(ilit) = oml(ii)
            liner(ilit) = 1.d0
        end do
    endif
!
! CALCUL DES COEFFICIENTS DE LA MATRICE ELASTIQUE
    call getres(mater, type, nomcmd)
    elb(1) = eb
    elb(2) = nub
    call parglr(nlit, elb, ea, nua, liner, omx, omy, rx, ry, hh,&
                bn11, bn12, bn22, bn33, bm11, bm12, bm22, bc11, bc22)
!
! E ET NU EQUIVALENTS EN FLEXION
    num = 2.d0*bn12/(bn11+bn22)
    em = (bn11+bn22)/(2.d0*hh)*(1.d0-num**2)
    nueq = 2.d0*bm12/(bm11+bm22)
    eeq = (bm11+bm22)*6.d0/hh**3 *(1.d0-nueq**2)
    par1 = eeq*hh / (1.d0 - nueq*nueq)
    par2 = par1*hh*hh/12.d0
    bm11 = par2
    bm12 = par2*nueq
    bm22 = par2
    bm33 = par2*(1.d0 - nueq)/2.d0
!
! MOMENT DE FISSURATION
    mf1x=((bn11*ft/eb-prex)*bm11+(prex*hh/2.d0-ft/eb*bc11)*bc11) / ( bn11*hh/2.d0-bc11)
    mf1y=((bn22*ft/eb-prey)*bm22+(prey*hh/2.d0-ft/eb*bc22)*bc22) / ( bn22*hh/2.d0-bc22)
    mf2x=((bn11*ft/eb-prex)*bm11+(-prex*hh/2.d0-ft/eb*bc11)*bc11)/ (-bn11*hh/2.d0-bc11)
    mf2y=((bn22*ft/eb-prey)*bm22+(-prey*hh/2.d0-ft/eb*bc22)*bc22)/ (-bn22*hh/2.d0-bc22)
!
! MOYENNE DANS CHAQUE DIRECTION
    mf1 = (mf1x+mf1y)/2.d0
    mf2 = (mf2x+mf2y)/2.d0
!
    call getvr8('BETON', 'BT1', iocc=1, scal=bt1, nbret=icis)
    call getvr8('BETON', 'BT2', iocc=1, scal=bt2, nbret=icis)
!
    if (icis .eq. 0) then
        call getvr8('BETON', 'OMT', iocc=1, scal=omt, nbret=icis)
        call getvr8('BETON', 'EAT', iocc=1, scal=eat, nbret=icis)
        if (icis .ne. 0) then
            bt1 = 5.d0/6.d0*hh/2.d0*(eb/(1.d0+nub)+eat*omt)
        else
            bt1 = 5.d0/6.d0*hh/2.d0*eb/(1.d0+nub)
        endif
        bt2 = bt1
    endif
!
!-----REMPLISSAGE DU MATERIAU
    call wkvect(mater//'.MATERIAU.NOMRC ', 'G V K32', 3, jlm)
    zk32(jlm ) =  'GLRC_DAMAGE'
    zk32(jlm+1) = 'ELAS_GLRC'
    zk32(jlm+2) = 'BPEL_BETON'

!---------ELASTIQUE---------------
    lonobj = 11
    call codent(2,'D0',K6)
    call wkvect(mater//'.CPT.'//K6//'.VALK', 'G V K16', 2*lonobj, jmelk)
    call jeecra(mater//'.CPT.'//K6//'.VALK', 'LONUTI',   lonobj)
    call wkvect(mater//'.CPT.'//K6//'.VALR', 'G V R',    lonobj, jmelr)
    call jeecra(mater//'.CPT.'//K6//'.VALR', 'LONUTI',   lonobj)
    call wkvect(mater//'.CPT.'//K6//'.VALC', 'G V C',    lonobj, jmelc)
    call jeecra(mater//'.CPT.'//K6//'.VALC', 'LONUTI',   0)
    zk16(jmelk) = 'E_M'
    zr(jmelr) = em
    zk16(jmelk+1) = 'NU_M'
    zr(jmelr+1) = num
    zk16(jmelk+2) = 'E_F'
    zr(jmelr+2) = eeq
    zk16(jmelk+3) = 'NU_F'
    zr(jmelr+3) = nueq
    zk16(jmelk+4) = 'BT1'
    zr(jmelr+4) = bt1
    zk16(jmelk+5) = 'BT2'
    zr(jmelr+5) = bt2
    zk16(jmelk+6) = 'RHO'
    zr(jmelr+6) = rho
    if (amora .gt. 0.0d0) then
        zk16(jmelk+7) = 'AMOR_ALPHA'
        zr(jmelr+7) = amora
    endif
    if (amorb .gt. 0.0d0) then
        zk16(jmelk+8) = 'AMOR_BETA'
        zr(jmelr+8) = amorb
    endif
    if (amorh .gt. 0.0d0) then
        zk16(jmelk+9) = 'AMOR_HYST'
        zr(jmelr+9 ) = amorh
    endif

!   -- alphat : coef. dilatation thermique :
    call getvr8(' ', 'ALPHA', iocc=1, scal=alphat, nbret=nalphat)
    if (nalphat .eq. 1) then
        zk16(jmelk+10) = 'ALPHA'
        zr(jmelr+10 ) = alphat
    endif


!---------BPEL_BETON--------------
    lonobj = 2
    call codent(3,'D0',K6)
    call wkvect(mater//'.CPT.'//K6//'.VALK', 'G V K16', 2*lonobj, jmelk)
    call jeecra(mater//'.CPT.'//K6//'.VALK', 'LONUTI',   lonobj)
    call wkvect(mater//'.CPT.'//K6//'.VALR', 'G V R',    lonobj, jmelr)
    call jeecra(mater//'.CPT.'//K6//'.VALR', 'LONUTI',   lonobj)
    call wkvect(mater//'.CPT.'//K6//'.VALC', 'G V C',    lonobj, jmelc)
    call jeecra(mater//'.CPT.'//K6//'.VALC', 'LONUTI',   0)
    zk16(jmelk )  = 'PERT_RETR'
    zr(jmelr )   = pretr
    zk16(jmelk+1) = 'PERT_FLUA'
    zr(jmelr+1 ) = pflua
!---------GLRC_DAMAGE---------------
    lonobj = 48
    lonuti = 35
    call codent(1,'D0',K6)
!
    call wkvect(mater//'.CPT.'//K6//'.VALK', 'G V K16', 2*lonobj, jmelk)
!
    call getvr8('BETON', 'MP1X', iocc=1, scal=mp1cst(1), nbret=icst)
!
    if (icst .eq. 0) then
        call jeecra(mater//'.CPT.'//K6//'.VALK', 'LONUTI', 59)
    else
        call jeecra(mater//'.CPT.'//K6//'.VALK', 'LONUTI', lonuti)
    endif
!
    call wkvect(mater//'.CPT.'//K6//'.VALR', 'G V R',  lonobj, jmelr)
    call jeecra(mater//'.CPT.'//K6//'.VALR', 'LONUTI', lonuti)
    call wkvect(mater//'.CPT.'//K6//'.VALC', 'G V C',  lonobj, jmelc)
    call jeecra(mater//'.CPT.'//K6//'.VALC', 'LONUTI', 0)
    ifon0 = jmelk + lonuti
    longf = 12
    ifon1 = ifon0 + longf
    zk16(jmelk )  = 'BN11    '
    zr(jmelr )   = bn11
    zk16(jmelk+1) = 'BN12    '
    zr(jmelr+1 ) = bn12
    zk16(jmelk+2) = 'BN22    '
    zr(jmelr+2 ) = bn22
    zk16(jmelk+3) = 'BN33    '
    zr(jmelr+3 ) = bn33
    zk16(jmelk+4) = 'MF1     '
    zr(jmelr+4 ) = mf1
    zk16(jmelk+5) = 'MF2     '
    zr(jmelr+5 ) = mf2
    zk16(jmelk+6) = 'QP1     '
    zr(jmelr+6 ) = qp1
    zk16(jmelk+7) = 'QP2     '
    zr(jmelr+7 ) = qp2
    zk16(jmelk+8) = 'GAMMA   '
    zr(jmelr+8 ) = gamma
    zk16(jmelk+9) = 'BT1     '
    zr(jmelr+9 ) = bt1
    zk16(jmelk+10) = 'BT2     '
    zr(jmelr+10 ) = bt2
    zk16(jmelk+11) = 'C1N1    '
    zr(jmelr+11 ) = cn(1,1)
    zk16(jmelk+12) = 'C1N2    '
    zr(jmelr+12 ) = cn(1,2)
    zk16(jmelk+13) = 'C1N3    '
    zr(jmelr+13 ) = cn(1,3)
    zk16(jmelk+14) = 'C2N1    '
    zr(jmelr+14 ) = cn(2,1)
    zk16(jmelk+15) = 'C2N2    '
    zr(jmelr+15 ) = cn(2,2)
    zk16(jmelk+16) = 'C2N3    '
    zr(jmelr+16 ) = cn(2,3)
    zk16(jmelk+17) = 'C1M1    '
    zr(jmelr+17 ) = cm(1,1)
    zk16(jmelk+18) = 'C1M2    '
    zr(jmelr+18 ) = cm(1,2)
    zk16(jmelk+19) = 'C1M3    '
    zr(jmelr+19 ) = cm(1,3)
    zk16(jmelk+20) = 'C2M1    '
    zr(jmelr+20 ) = cm(2,1)
    zk16(jmelk+21) = 'C2M2    '
    zr(jmelr+21 ) = cm(2,2)
    zk16(jmelk+22) = 'C2M3    '
    zr(jmelr+22 ) = cm(2,3)
    zk16(jmelk+23) = 'MAXMP1  '
    zr(jmelr+23 ) = 0.0d0
    zk16(jmelk+24) = 'MAXMP2  '
    zr(jmelr+24 ) = 0.0d0
    zk16(jmelk+25) = 'MINMP1  '
    zr(jmelr+25 ) = 0.0d0
    zk16(jmelk+26) = 'MINMP2  '
    zr(jmelr+26 ) = 0.0d0
    zk16(jmelk+27) = 'NORMM  '
    zr(jmelr+27 ) = 1.0d0
    zk16(jmelk+27) = 'NORMN  '
    zr(jmelr+27 ) = 1.0d0
!---------IMPRESSION-------------
    if (niv .gt. 0) then
        write (ifm,*) 'PARAMETRES HOMOGENEISES POUR GLRC_DAMAGE'
        write (ifm,*) 'MODULE D YOUNG ET COEFFICIENT DE POISSON EFFECTIFS EN MEMBRANE:'
        write (ifm,*) 'E_M =  :',em
        write (ifm,*) 'NU_M =  :',num
        write (ifm,*) 'MODULE D YOUNG ET COEFFICIENT DE POISSON EFFECTIFS EN FLEXION:'
        write (ifm,*) 'E_F =  :',eeq
        write (ifm,*) 'NU_F =  :',nueq
        write (ifm,*) 'MATRICE DE D ELASTICITE EN MEMBRANE:'
        write (ifm,*) 'BN11, BN12, BN22, BN33 =   :', bn11, ' ', bn12, ' ', bn22,' ', bn33
        write (ifm,*) 'MATRICE DE D ELASTICITE EN FLEXION:'
        write (ifm,*) 'BM11, BM12, BM22, BM33 =   :', bm11, ' ', bm12, ' ', bm22,' ', bm33
        if (icis .eq. 0) then
            write (ifm,*) 'MATRICE DE D ELASTICITE EN CISAILLEMENT TRANSVERSE:'
            write (ifm,*) 'BT1, BT2 =   :', bt1, ' ', bt2
        endif
        write (ifm,*) 'MOMENTS DE FISSURATION:'
        write (ifm,*) 'MF1, MF2 =   :', mf1, ' ', mf2
        write (ifm,*) 'RAPPORT DE PENTE:'
        write (ifm,*) 'GAMMA, QP1, QP2 =   :', gamma, ' ', qp1, ' ', qp2
        write (ifm,*) 'COEFFICIENT DE PRAGER EN MEMBRANE:'
        write (ifm,*) 'C1N1, C1N2, C1N3 =   :', cn(1,1), ' ', cn(1,2), ' ', cn(1,3)
        write (ifm,*) 'C2N1, C2N2, C2N3 =   :', cn(2,1), ' ', cn(2,2), ' ', cn(2,3)
        write (ifm,*) 'COEFFICIENT DE PRAGER EN FLEXION:'
        write (ifm,*) 'C1M1, C1M2, C1M3 =   :', cm(1,1), ' ', cm(1,2), ' ', cm(1,3)
        write (ifm,*) 'C2M1, C2M2, C2M3 =   :', cm(2,1), ' ', cm(2,2), ' ', cm(2,3)
    endif
!--------CREER LES FONCTIONS SEUILS---------
!
!--------L UTILISATEUR A ENTRE DES CONSTANTES
!
    call getvr8('BETON', 'MP1Y', iocc=1, scal=mp1cst(2), nbret=icst)
    call getvr8('BETON', 'MP2X', iocc=1, scal=mp2cst(1), nbret=icst)
    call getvr8('BETON', 'MP2Y', iocc=1, scal=mp2cst(2), nbret=icst)
!
    if (icst .eq. 0) then
!--------L UTILISATEUR A ENTRE DES FONCTIONS
!
        call getvid('BETON', 'MP1X_FO', iocc=1, scal=fsncx, nbret=impf)
        call getvid('BETON', 'MP2X_FO', iocc=1, scal=fincx, nbret=impf)
        call getvid('BETON', 'MP1Y_FO', iocc=1, scal=fsncy, nbret=impf)
        call getvid('BETON', 'MP2Y_FO', iocc=1, scal=fincy, nbret=impf)
!
        if (impf .eq. 1) then
            call gcncon('_', fscxd)
            call gcncon('_', fscyd)
            call gcncon('_', fscxd2)
            call gcncon('_', fscyd2)
            call gcncon('_', ficxd)
            call gcncon('_', ficyd)
            call gcncon('_', ficxd2)
            call gcncon('_', ficyd2)
!-----Mx/Nx -------------
            call mocon2('X', fc, sy, hh, nlit, omx, rx, fsncx, fincx, fscxd,&
                        ficxd, fscxd2, ficxd2, prex)
!-----My/Ny -------------
            call mocon2('Y', fc, sy, hh, nlit, omy, ry, fsncy, fincy, fscyd,&
                        ficyd, fscyd2, ficyd2, prey)
        else
!--------L UTILISATEUR N A RIEN RENSEIGNE
!
            call gcncon('_', fsncx)
            call gcncon('_', fsncy)
            call gcncon('_', fincx)
            call gcncon('_', fincy)
            call gcncon('_', fscxd)
            call gcncon('_', fscyd)
            call gcncon('_', fscxd2)
            call gcncon('_', fscyd2)
            call gcncon('_', ficxd)
            call gcncon('_', ficyd)
            call gcncon('_', ficxd2)
            call gcncon('_', ficyd2)
!-----Mx/Nx -------------
            call moconm('X', fc, sy, hh, nlit, omx, rx, fsncx, fincx, fscxd,&
                        ficxd, fscxd2, ficxd2, prex)
!-----My/Ny -------------
            call moconm('Y', fc, sy, hh, nlit, omy, ry, fsncy, fincy, fscyd,&
                        ficyd, fscyd2, ficyd2, prey)
        endif
!
        zk16(ifon0 ) = 'FMEX1   '
        zk16(ifon1 ) = fsncx
        zk16(ifon0+1) = 'FMEY1   '
        zk16(ifon1+1) = fsncy
        zk16(ifon0+2) = 'DFMEX1  '
        zk16(ifon1+2) = fscxd
        zk16(ifon0+3) = 'DFMEY1  '
        zk16(ifon1+3) = fscyd
        zk16(ifon0+4) = 'DDFMEX1 '
        zk16(ifon1+4) = fscxd2
        zk16(ifon0+5) = 'DDFMEY1 '
        zk16(ifon1+5) = fscyd2
        zk16(ifon0+6) = 'FMEX2   '
        zk16(ifon1+6) = fincx
        zk16(ifon0+7) = 'FMEY2   '
        zk16(ifon1+7) = fincy
        zk16(ifon0+8) = 'DFMEX2  '
        zk16(ifon1+8) = ficxd
        zk16(ifon0+9) = 'DFMEY2  '
        zk16(ifon1+9) = ficyd
        zk16(ifon0+10) = 'DDFMEX2 '
        zk16(ifon1+10) = ficxd2
        zk16(ifon0+11) = 'DDFMEY2 '
        zk16(ifon1+11) = ficyd2
!
!---------IMPRESSION-------------
        if (niv .gt. 0) then
            write (ifm,*) 'FONCTIONS SEUIL POUR GLRC:'
            call foimpr(fsncx, 3, ifm, 0, k8b)
            call foimpr(fsncy, 3, ifm, 0, k8b)
            call foimpr(fincx, 3, ifm, 0, k8b)
            call foimpr(fincy, 3, ifm, 0, k8b)
!
            write (ifm,*) 'DERIVEES PREMIERES :'
            call foimpr(fscxd, 3, ifm, 0, k8b)
            call foimpr(fscyd, 3, ifm, 0, k8b)
            call foimpr(ficxd, 3, ifm, 0, k8b)
            call foimpr(ficyd, 3, ifm, 0, k8b)
!
            write (ifm,*) 'DERIVEES SECONDES :'
            call foimpr(fscxd2, 3, ifm, 0, k8b)
            call foimpr(fscyd2, 3, ifm, 0, k8b)
            call foimpr(ficxd2, 3, ifm, 0, k8b)
            call foimpr(ficyd2, 3, ifm, 0, k8b)
        endif
!
        nomres(1) = 'FMEX1'
        fon(1) = fsncx
        nomres(2) = 'FMEX2'
        fon(2) = fincx
        nomres(3) = 'FMEY1'
        fon(3) = fsncy
        nomres(4) = 'FMEY2'
        fon(4) = fincy
    endif
!
!------CALCULER MAXMP,MINMP,NORMM,NORMN-----------------
    do ii = 1, 2
        if (icst .eq. 0) then
            k8b = 'X '
            call fointe('F', fon(2*(ii-1)+1), 1, ['X '], [0.0d0], mp1n0, ibid)
            call fointe('F', fon(2*ii ), 1, ['X '], [0.0d0], mp2n0, ibid)
            call mmfonc(fon(2*(ii-1)+1), aux, maxmp(ii))
            call mmfonc(fon(2*ii), minmp(ii), aux)
            if ((mp1n0 .lt. 0.d0) .or. (mp2n0 .gt. 0.d0) .or.&
                (maxmp(ii) -minmp(ii) .le. 0.d0)) then
                call utmess('F', 'ELEMENTS_87')
            endif
        else
            maxmp(ii)=mp1cst(ii)
            minmp(ii)=mp2cst(ii)
        endif
    end do
!
    normm=0.5d0*max(maxmp(1)-minmp(1),maxmp(2)-minmp(2))
!
    do ii = 1, 2
        if (icst .eq. 0) then
            nmax0 = 1.0d20
            nmin0 = -1.0d20
            call interf(mater, nomres(2*(ii-1)+1), nomres(2*ii), normm, nmin0, nmin(ii))
            call interf(mater, nomres(2*(ii-1)+1), nomres(2*ii), normm, nmax0, nmax(ii))
        else
            nmax(ii)=0.d0
            nmin(ii)=0.d0
        endif
    end do
!
    normn=0.5d0*max(abs(nmax(1)-nmin(1)),abs(nmax(2)-nmin(2)))
!
    zk16(jmelk+23) = 'MAXMP1  '
    zr(jmelr+23 ) = maxmp(1)
    zk16(jmelk+24) = 'MAXMP2  '
    zr(jmelr+24 ) = maxmp(2)
    zk16(jmelk+25) = 'MINMP1  '
    zr(jmelr+25 ) = minmp(1)
    zk16(jmelk+26) = 'MINMP2  '
    zr(jmelr+26 ) = minmp(2)
    zk16(jmelk+27) = 'NORMM  '
    zr(jmelr+27 ) = normm
    zk16(jmelk+28) = 'NORMN  '
    zr(jmelr+28 ) = normn
    zk16(jmelk+29) = 'EPAIS  '
    zr(jmelr+29 ) = hh
    zk16(jmelk+30) = 'BM11    '
    zr(jmelr+30 ) = bm11
    zk16(jmelk+31) = 'BM12    '
    zr(jmelr+31 ) = bm12
    zk16(jmelk+32) = 'BM22    '
    zr(jmelr+32 ) = bm22
    zk16(jmelk+33) = 'BM33    '
    zr(jmelr+33 ) = bm33
    zk16(jmelk+34) = 'MPCST   '
    if (icst .ne. 0) then
        zr(jmelr+34 ) = 0.d0
    else
        zr(jmelr+34 ) = 1.d0
    endif
!
    call jedema()
end subroutine
