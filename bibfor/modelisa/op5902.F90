subroutine op5902(nboccp, compor)
    implicit none
! ======================================================================
! COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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
! person_in_charge: jean-michel.proix at edf.fr
!
!     COMMANDE:  DEFI_COMPOR MOT-CLE POLYCRISTAL
!
#include "jeveux.h"
!
#include "asterc/getvid.h"
#include "asterc/getvr8.h"
#include "asterc/getvtx.h"
#include "asterc/indik8.h"
#include "asterfort/assert.h"
#include "asterfort/codent.h"
#include "asterfort/eulnau.h"
#include "asterfort/jedema.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/u2mesg.h"
#include "asterfort/u2mesr.h"
#include "asterfort/wkvect.h"
    character(len=8) :: compor, mono, chaine
    character(len=16) :: kbid, loca, noms(6), nomvar(100)
    real(kind=8) :: fvol, orie(3), dl, da, euler(3), fvolt
    integer :: iocc, nloc, nboccp, ndl, nda, itbint, nums(3)
    integer :: i, nmono, imk, imi, ipk, ipi, ipr, iorie, irra
    integer :: ncpri, ncprk, ncprr, jcprk, jcprr, jcpri, nvit, lmk, ifvol, ipl
    integer :: imono, nbmono, nvloc, indvar
    integer :: nbsyst, nbsysm
    integer :: iarg
!
!
!
    call jemarq()
!
    call getvtx(' ', 'LOCALISATION', 0, iarg, 1,&
                loca, nloc)
    dl=0.d0
    da=0.d0
    nvloc=0
    if (loca .eq. 'BETA') then
        call getvr8(' ', 'DL', 0, iarg, 1,&
                    dl, ndl)
        call getvr8(' ', 'DA', 0, iarg, 1,&
                    da, nda)
        nvloc=2
    endif
!
!
!     organisation de CPRI :
!     1 : TYPE =2 pour POLYCRISTAL
!     2 : NBPHAS pour POLYCRISTAL
!     3 : NVITOT pour POLYCRISTAL
!     4 : NOMBRE DE MONOCRISTAUX différents
!     5 : NBFAMILLES DE SYS GLIS pour Phase 1
!     6 : Numero du MONO 1
!     7 : NVI du Mono 1
!     8 : NBFAMILLES DE SYS GLIS pour Phase 2
!     9 : Numero du MONO 2
!     10 : NVI du Mono 2
!      etc...
!     avant dernier : dimension de CPRK
!     nombre de paramètres de localisation
!
    ncpri=4+3*nboccp+1+1+1
    call wkvect(compor//'.CPRI', 'G V I', ncpri, ipi)
    zi(ipi)=2
    zi(ipi+1)=nboccp
    call wkvect('&&OP0059.LISTEMONO', 'V V K8', nboccp, ipl)
!
    nbmono=0
    ncprk=0
!
    do 13 iocc = 1, nboccp
        call getvid('POLYCRISTAL', 'MONOCRISTAL', iocc, iarg, 1,&
                    mono, nmono)
!        On ne stocke pas les doublons
        imono=indik8(zk8(ipl),mono,1,nbmono)
        if (imono .eq. 0) then
            nbmono=nbmono+1
            zk8(ipl-1+nbmono)=mono
            zi(ipi-1+4+3*(iocc-1)+2)=nbmono
            call jelira(mono//'.CPRK', 'LONMAX', lmk, kbid)
            ncprk=ncprk+lmk+2
        else
            zi(ipi-1+4+3*(iocc-1)+2)=imono
        endif
13  end do
    ncprk=ncprk+1
    if (nbmono .gt. 5) then
        call u2mesg('F', 'COMPOR2_16', 0, ' ', 1,&
                    itbint, 0, 0.d0)
    else
        zi(ipi-1+4)=nbmono
    endif
    irra=0
!
!     organisation de CPRK :
!     On ne stocke que les monocristaux DIFFERENTS
!     1   : Nom méthode localisation
!     2   : Nom Monocristal 1 + NBFAM + CPRK du monocristal 1
!     n+2 : Nom Monocristal 2 + NBFAM + CPRK du monocristal 2
!     ..: etc...
    call wkvect(compor//'.CPRK', 'G V K24', ncprk, ipk)
    jcprk=1
    itbint=0
    do 15 imono = 1, nbmono
        mono=zk8(ipl-1+imono)
        call jelira(mono//'.CPRK', 'LONMAX', lmk, kbid)
        call jeveuo(mono//'.CPRK', 'L', imk)
        call jeveuo(mono//'.CPRI', 'L', imi)
!        RECOPIE DU VECTEUR K16 DU MONOCRISTAL DANS CELUI DU POLY
        zk24(ipk-1+jcprk+1)=mono
        write(zk24(ipk-1+jcprk+2),'(I24)') zi(imi-1+5)
        do 14 i = 1, lmk
            zk24(ipk-1+jcprk+2+i)=zk24(imk-1+i)
14      continue
        jcprk=jcprk+lmk+2
        if (zk24(imk-1+3) .eq. 'MONO_DD_CC_IRRA') irra=1
        if (zk24(imk-1+3) .eq. 'MONO_DD_CFC_IRRA') irra=2
!
15  end do
!
    ncprr=4*nboccp+2
!
    call wkvect(compor//'.CPRR', 'G V R', ncprr, ipr)
    jcprr=0
    jcpri=4
    nvit=0
    fvolt=0.d0
    nbsyst=0
    nbsysm=0
    do 16 iocc = 1, nboccp
        imono=zi(ipi-1+4+3*(iocc-1)+2)
        mono=zk8(ipl-1+imono)
        call jeveuo(mono//'.CPRI', 'L', imi)
        zi(ipi-1+jcpri+1)=zi(imi-1+5)
        zi(ipi-1+jcpri+3)=zi(imi-1+7)
!
!        NVI DU MONOCRISTAL : 6+4*NS + 3 + (NS SI IRRA) +3 )
!       (EVP + NS(ALPHAS, GAMMAS, PS) + RHO_IRRA + 3)
!        NOMBRE DE VAR INT POLYCRISTAL SANS IRRA :
!         7   + 6*NG  +NG*(NS*(ALPHAS,GAMMAS,PS))+6*NG+1
!        EVP+P+EVPG*NG+NG*(NS*(ALPHAS,GAMMAS,PS))+SIG*NG+1)
!        =7   + 6*NG  +NG*(NMONO-9)+6*NG+1=NG*(NMONO+3)+8
!        NOMBRE DE VAR INT POLYCRISTAL AVEC IRRA :
!         7   + 6*NG  +NG*(NS*(ALPHAS,GAMMAS,PS))+12*NG+6*NG+1
!        =7   + 6*NG  +NG*(NMONO-9)+6*NG+1=NG*(NMONO+3)+8
!
        nbsysm=max(nbsysm,zi(imi-1+8))
        nbsyst=nbsyst+zi(imi-1+8)
!        ON ENLEVE LES TAUS ET LES 3 VARIABLES INTERNES P,SCLIV,INDIC
        nvit=nvit+zi(imi-1+7)+3-zi(imi-1+8)
        jcpri=jcpri+3
        call getvr8('POLYCRISTAL', 'FRAC_VOL', iocc, iarg, 1,&
                    fvol, ifvol)
        call getvr8('POLYCRISTAL', 'ANGL_REP', iocc, iarg, 3,&
                    orie, iorie)
        if (iorie .eq. 0) then
            call getvr8('POLYCRISTAL', 'ANGL_EULER', iocc, iarg, 3,&
                        euler, iorie)
            call eulnau(euler, orie)
        endif
        fvolt=fvolt+fvol
        zr(ipr-1+jcprr+1)=fvol
        zr(ipr-1+jcprr+2)=orie(1)
        zr(ipr-1+jcprr+3)=orie(2)
        zr(ipr-1+jcprr+4)=orie(3)
        jcprr=jcprr+4
16  end do
!
    if (abs(fvolt-1.d0) .gt. 1.d-3) then
        call u2mesr('F', 'COMPOR2_8', 1, fvolt)
    endif
!
    zr(ipr-1+jcprr+1)=dl
    zr(ipr-1+jcprr+2)=da
!
!      NOMBRE DE VAR INT TOTAL + 8 (TENSEUR B OU EVP + NORME+INDIC)
    nvit=nvit+8
    zi(ipi-1+3)=nvit
    zi(ipi-1+ncpri-2)=jcprk
    zi(ipi-1+ncpri-1)=nvloc
!
    zk24(ipk)=loca
!
!     IMPRESSION DES VARIABLES INTERNES
    indvar=0
    noms(1)='POLYCRISTAL'
    noms(2)=loca
    nums(1)=nboccp
    nums(2)=nbmono
    nums(3)=nvit
    call u2mesg('I', 'COMPOR2_27', 2, noms, 3,&
                nums, 0, 0.d0)
!
    nomvar(1)='EPSPXX'
    nomvar(2)='EPSPYY'
    nomvar(3)='EPSPZZ'
    nomvar(4)='EPSPXY'
    nomvar(5)='EPSPXZ'
    nomvar(6)='EPSPYZ'
    nomvar(7)='EPSPEQ'
    do 558 i = 1, 7
        call u2mesg('I', 'COMPOR2_24', 1, nomvar(i), 1,&
                    i, 0, 0.d0)
558  end do
    indvar=indvar+7
    call u2mesg('I', 'COMPOR2_28', 0, ' ', 1,&
                indvar+1, 0, 0.d0)
    nomvar(1)='EPSPXX(GRAIN_I)'
    nomvar(2)='EPSPYY(GRAIN_I)'
    nomvar(3)='EPSPZZ(GRAIN_I)'
    nomvar(4)='EPSPXY(GRAIN_I)'
    nomvar(5)='EPSPXZ(GRAIN_I)'
    nomvar(6)='EPSPYZ(GRAIN_I)'
    do 559 i = 1, 6
        call u2mesg('I', 'COMPOR2_24', 1, nomvar(i), 1,&
                    indvar+i, 0, 0.d0)
559  end do
    indvar=indvar+6*nboccp
    call u2mesg('I', 'COMPOR2_30', 0, ' ', 1,&
                indvar, 0, 0.d0)
!
    call u2mesg('I', 'COMPOR2_28', 0, ' ', 1,&
                indvar+1, 0, 0.d0)
!
    do 554 i = 1, nbsysm
        call codent(i, 'G', chaine)
        nomvar(3*i-2)='ALPHA'//chaine
        nomvar(3*i-1)='GAMMA'//chaine
        nomvar(3*i )='P'//chaine
554  end do
    do 556 i = 1, 3*nbsysm
        call u2mesg('I', 'COMPOR2_24', 1, nomvar(i), 1,&
                    indvar+i, 0, 0.d0)
556  end do
!
    indvar=indvar+3*nbsyst
!
    call u2mesg('I', 'COMPOR2_30', 0, ' ', 1,&
                indvar, 0, 0.d0)
!
    if (irra .eq. 1) then
        call u2mesg('I', 'COMPOR2_28', 0, ' ', 1,&
                    indvar+1, 0, 0.d0)
        do 557 i = 1, 12
            call codent(i, 'G', chaine)
            nomvar(i)='RHO_IRRA_'//chaine
557      continue
        do 560 i = 1, 12
            call u2mesg('I', 'COMPOR2_24', 1, nomvar(i), 1,&
                        indvar+i, 0, 0.d0)
560      continue
        indvar=indvar+12*nboccp
        call u2mesg('I', 'COMPOR2_30', 0, ' ', 1,&
                    indvar, 0, 0.d0)
    endif
!
    if (irra .eq. 2) then
        call u2mesg('I', 'COMPOR2_28', 0, ' ', 1,&
                    indvar+1, 0, 0.d0)
        do 571 i = 1, 12
            call codent(i, 'G', chaine)
            nomvar(i)='RHO_LOOPS_'//chaine
571      continue
        do 572 i = 1, 12
            call codent(i, 'G', chaine)
            nomvar(12+i)='PHI_VOIDS_'//chaine
572      continue
        do 573 i = 1, 24
            call u2mesg('I', 'COMPOR2_24', 1, nomvar(i), 1,&
                        indvar+i, 0, 0.d0)
573      continue
        indvar=indvar+24*nboccp
        call u2mesg('I', 'COMPOR2_30', 0, ' ', 1,&
                    indvar, 0, 0.d0)
    endif
!
    call u2mesg('I', 'COMPOR2_28', 0, ' ', 1,&
                indvar+1, 0, 0.d0)
!
    do 555 i = 1, 6
        nomvar(1)='SIGMAXX(GRAIN_I)'
        nomvar(2)='SIGMAYY(GRAIN_I)'
        nomvar(3)='SIGMAZZ(GRAIN_I)'
        nomvar(4)='SIGMAXY(GRAIN_I)'
        nomvar(5)='SIGMAXZ(GRAIN_I)'
        nomvar(6)='SIGMAYZ(GRAIN_I)'
555  end do
    do 561 i = 1, 6
        call u2mesg('I', 'COMPOR2_24', 1, nomvar(i), 1,&
                    indvar+i, 0, 0.d0)
561  end do
    indvar=indvar+6*nboccp
    call u2mesg('I', 'COMPOR2_30', 0, ' ', 1,&
                indvar, 0, 0.d0)
    indvar=indvar+1
    nomvar(1)='INDIPLAS'
    call u2mesg('I', 'COMPOR2_29', 1, nomvar(1), 1,&
                nvit, 0, 0.d0)
    call assert(indvar.eq.nvit)
! FIN ------------------------------------------------------------------
    call jedema()
end subroutine
