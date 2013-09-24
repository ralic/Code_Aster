subroutine pmdorc(compor, carcri, nbvari, incela)
    implicit none
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: jean-michel.proix at edf.fr
!-----------------------------------------------------------------------
!           OPERATEUR    CALC_POINT_MAT : LECTURE COMPOR ET CARCRI
!-----------------------------------------------------------------------
!      IDEM NMDORC MAIS SANS MODELEK
! ----------------------------------------------------------------------
! OUT COMPOR  : OBJET COMPOR(8) DECRIVANT LE TYPE DE COMPORTEMENT
! OUT CARCRI  : OBJET CARCRI(13) CRITERES DE CONVERGENCE LOCAUX
! OUT NBVARI  : NOMBRE DE VARIABLE INTERNES
! OUT k       : =1 si COMP_INCR, =2 si COMP_ELAS
#include "jeveux.h"
#include "asterc/getexm.h"
#include "asterc/getfac.h"
#include "asterc/lcalgo.h"
#include "asterc/lccree.h"
#include "asterc/lcinfo.h"
#include "asterc/lctest.h"
#include "asterc/zaswri.h"
#include "asterfort/getvid.h"
#include "asterfort/getvis.h"
#include "asterfort/getvr8.h"
#include "asterfort/getvtx.h"
#include "asterfort/imvari.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/utlcal.h"
#include "asterfort/utmess.h"
    integer :: iret, n1, nbvari, k, icpri, typtgt, exits
    integer :: ncomel, numlc, iteint, itepas, itdebo, nbocc, irett
    integer :: nunit, indimp, ncmpma, dimaki, dimanv, ii, incela
    integer :: nbocc1, nbocc2, nbocc3
!    DIMAKI = DIMENSION MAX DE LA LISTE DES RELATIONS KIT
    parameter (dimaki=9)
!    DIMANV = DIMENSION MAX DE LA LISTE DU NOMBRE DE VAR INT EN THM
    parameter (dimanv=4)
    parameter (ncmpma=7+dimaki+dimanv)
    character(len=8) :: sdcomp, tavari
    character(len=16) :: compor(ncmpma), comp, comcod, algo, lcomel(5)
    character(len=16) :: moclef(2)
    character(len=16) :: tymatg, texte(2), txcp, defo
    character(len=16) :: nomsub
    character(len=128) :: nomlib
    real(kind=8) :: carcri(13), resi, algor
    real(kind=8) :: resid, pert, theta, tseuil, tolrad
    logical :: iszmat
    save indimp
    data indimp /1/
!-----------------------------------------------------------------------
!
    call jemarq()
    moclef(1) = 'COMPORTEMENT'
    call getfac(moclef(1), nbocc)
    k=1

    iszmat = .false.
    call getvtx(moclef(1), 'RELATION', iocc=1, scal=comp, nbret=n1)
    ncomel=1
    lcomel(ncomel)=comp
    txcp='ANALYTIQUE'
!
!     APPEL A LCINFO POUR RECUPERER LE NOMBRE DE VARIABLES INTERNES
    call lccree(ncomel, lcomel, comcod)
    call lcinfo(comcod, numlc, nbvari)
!
!     NOMS DES VARIABLES INTERNES
    if (indimp .eq. 1) then
        call imvari(moclef(1), 1, ncomel, lcomel, comcod,&
                    nbvari, tavari)
    endif
    indimp=0
!
    call getvtx(moclef(1), 'DEFORMATION', iocc=1, scal=defo, nbret=n1)
!     VERIF QUE DEFO EST POSSIBLE POUR COMP
    call lctest(comcod, 'DEFORMATION', defo, iret)
    if (iret .eq. 0) then
        texte(1)=defo
        texte(2)=comp
        call utmess('F', 'COMPOR1_44', nk=2, valk=texte)
    endif
    if (defo .eq. 'SIMO_MIEHE') nbvari=nbvari+6
!
!     CAS PARTICULIER DU MONOCRISTAL
    if (comp(1:8) .eq. 'MONOCRIS') then
        call getvid(moclef(1), 'COMPOR', iocc=1, scal=sdcomp, nbret=n1)
        call jeveuo(sdcomp//'.CPRI', 'L', icpri)
        nbvari=zi(icpri-1+3)
        compor(7) = sdcomp
        if (defo .eq. 'SIMO_MIEHE') nbvari=nbvari+9+9
    else if (comp(1:8).eq.'POLYCRIS') then
        call getvid(moclef(1), 'COMPOR', iocc=1, scal=sdcomp, nbret=n1)
        call jeveuo(sdcomp//'.CPRI', 'L', icpri)
        nbvari=zi(icpri-1+3)
        compor(7) = sdcomp
    endif
!
    if (comp(1:4) .eq. 'ZMAT') then
        iszmat = .true.
        call getvis(moclef(1), 'NB_VARI', iocc=1, scal=nbvari, nbret=n1)
        call getvis(moclef(1), 'UNITE', iocc=1, scal=nunit, nbret=n1)
        write (compor(7),'(I16)') nunit
    else if ((comp.eq.'UMAT').or.(comp.eq.'MFRONT')) then
        call getvis(moclef(1), 'NB_VARI', iocc=1, scal=nbvari, nbret=n1)
!       POUR LES COMPORTEMENTS UMAT
!       ON STOCKE LA LIB DANS KIT1-KIT8 (128 CARACTERES)
!       ET LA SUBROUTINE DANS KIT9
        call getvtx(moclef(1), 'LIBRAIRIE', iocc=1, scal=nomlib, nbret=n1)
        call getvtx(moclef(1), 'NOM_ROUTINE', iocc=1, scal=nomsub, nbret=n1)
        do 30 ii = 1, dimaki-1
            compor(ii+7) = nomlib(16*(ii-1)+1:16*ii)
30      continue
        compor(dimaki+7) = nomsub
!       POUR EVITER DE PLANTER DANS LC0050 / TECAEL
        comp(9:16)='OP0033__'
    endif

!   determination du caratere incremental ou elastique    
    call lctest(comcod, 'PROPRIETES', 'COMP_ELAS', iret)
    if (iret .eq. 0) then
        moclef(2)='COMP_INCR'
        incela=1
    else
        moclef(2)='COMP_ELAS'
        incela=2
        
!       exceptions

        call getfac('SIGM_INIT', nbocc1)
        call getfac('EPSI_INIT', nbocc2)
        call getfac('VARI_INIT', nbocc3)
        if ((nbocc1+nbocc2+nbocc3)>0) then
            moclef(2)='COMP_INCR'
            incela=1
        endif
    endif
    
    compor(1)=comp
    write (compor(2),'(I16)') nbvari
    compor(3)=defo
    compor(4)=moclef(2)
    compor(5)=txcp
    write (compor(6),'(I16)') numlc
!
!     ALGORITHME D'INTEGRATION
    call getvtx(moclef(1), 'ALGO_INTE', iocc=1, scal=algo, nbret=iret)
    if (iret .eq. 0) then
!        LOI DE COMPORTEMENT (1ERE VALEUR DE LA LISTE)
        call lcalgo(comcod, algo)
    endif
!
!     CRITERES DE CONVERGENCE
    call getvr8(moclef(1), 'RESI_INTE_RELA', iocc=1, scal=resi, nbret=iret)
    call getvis(moclef(1), 'ITER_INTE_MAXI', iocc=1, scal=iteint, nbret=iret)
!
    itepas = 0
    if (k .eq. 1) then
        call getvis(moclef(1), 'ITER_INTE_PAS', iocc=1, scal=itepas, nbret=iret)
    endif
!
!     CPLAN DEBORST  ET COMP1D DEBORST INUTILES AVEC SUPPORT='POINT'
    resid=1.d-6
    pert=0.d0
    itdebo=1
!     PASSAGE NOM ALGO -> IDENTIFICATEUR (VALEUR REELLE)
    call utlcal('NOM_VALE', algo, algor)
    typtgt = 0
    if (moclef(1) .eq. 'COMPORTEMENT') then
        exits = getexm(moclef(1),'TYPE_MATR_TANG')
        if (exits .eq. 1) then
!        dans ZR(JVALV+1) on stocke le type de matrice tgte
            call getvtx(moclef(1), 'TYPE_MATR_TANG', iocc=1, scal=tymatg, nbret=iret)
            if (iret .eq. 0) then
                typtgt = 0
            else
                if (tymatg .eq. 'PERTURBATION') then
                    typtgt = 1
                    call getvr8(moclef(1), 'VALE_PERT_RELA', iocc=1, scal=pert, nbret=iret)
                else if (tymatg.eq.'VERIFICATION') then
                    typtgt = 2
                    call getvr8(moclef(1), 'VALE_PERT_RELA', iocc=1, scal=pert, nbret=iret)
                endif
!              Verif que TYMATG est possible pour COMP
                call lctest(comcod, 'TYPE_MATR_TANG', tymatg, irett)
                if (irett .eq. 0) then
                    texte(1)=tymatg
                    texte(2)=comp
                    call utmess('F', 'COMPOR1_46', nk=2, valk=texte)
                endif
            endif
        endif
    endif
!
    tseuil=-10.d0
!
!     TOLERANCE POUR LE CRITERE DE RADIALITE
    if (moclef(1) .eq. 'COMPORTEMENT') then
        if (typtgt .eq. 0) then
            call getvr8(moclef(1), 'RESI_RADI_RELA', iocc=1, scal=tolrad, nbret=iret)
            if (iret .ne. 0) then
                tseuil=tolrad
            else
                tseuil=-10.d0
            endif
        endif
    endif
!
    if (k .eq. 1) then
        call getvr8(moclef(1), 'PARM_THETA', iocc=1, scal=theta, nbret=iret)
    else
        theta=1.d0
    endif
    carcri(1)=iteint
    carcri(2)=typtgt
    carcri(3)=resi
    carcri(4)=theta
    carcri(5)=itepas
    carcri(6)=algor
    carcri(7)=pert
    carcri(8)=resid
    carcri(9)=itdebo
    carcri(10)=tseuil
    carcri(11)=0.d0
    carcri(12)=0.d0
    carcri(13)=0.d0
!
!     SI ZMAT, ON REINITIALISE LES ZASTER_HANDLER POUR FORCER
!     LA RELECTURE DES FICHIERS DECRIVANT LES COMPORTEMENTS
    if (iszmat) then
        call zaswri()
    endif
!
    call jedema()
!
end subroutine
