subroutine nmpipe(modele, ligrpi, cartyp, careta, mate,&
                  compor, resoco, valinc, depdel, ddepl0,&
                  ddepl1, tau, nbeffe, eta, pilcvg,&
                  typpil, carele)
!
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
! person_in_charge: mickael.abbas at edf.fr
!
    implicit none
#include "jeveux.h"
#include "asterc/r8vide.h"
#include "asterfort/assert.h"
#include "asterfort/calcul.h"
#include "asterfort/celces.h"
#include "asterfort/cesexi.h"
#include "asterfort/dbgcal.h"
#include "asterfort/detrsd.h"
#include "asterfort/infdbg.h"
#include "asterfort/inical.h"
#include "asterfort/jedema.h"
#include "asterfort/jeexin.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/mecact.h"
#include "asterfort/megeom.h"
#include "asterfort/nmchex.h"
#include "asterfort/pipere.h"
#include "asterfort/sdmpic.h"
#include "asterfort/wkvect.h"
    integer :: pilcvg, nbeffe
    real(kind=8) :: tau, eta(2)
    character(len=24) :: typpil
    character(len=19) :: ddepl0, ddepl1
    character(len=19) :: ligrpi, cartyp, careta
    character(len=24) :: modele, mate, compor, carele
    character(len=19) :: depdel, valinc(*)
    character(len=24) :: resoco
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (ALGORITHME - PILOTAGE)
!
! RESOLUTION DE L'EQUATION DE PILOTAGE PAR PREDICTION ELASTIQUE OU
! DEFORMATION
!
! ----------------------------------------------------------------------
!
!
! IN  MODELE : MODELE
! IN  LIGRPI : LIGREL DES MAILLES CONTROLEES PAR LE PILOTAGE
! IN  CARTYP : CARTE CONTENANT LE TYPE DE PILOTAGE
! IN  MATE   : MATERIAU
! IN  CARELE : CARACTERISTIQUES DES ELEMENTS DE STRUCTURE
! IN  COMPOR : COMPORTEMENT
! IN  RESOCO : SD DE TRAITEMENT NUMERIQUE DU CONTACT
! IN  DEPDEL : INCREMENT DE DEPLACEMENT
! IN  DDEPL0 : VARIATION DE DEPLACEMENT K-1.F0
! IN  DDEPL1 : VARIATION DE DEPLACEMENT K-1.F1
! IN  VALINC : VARIABLE CHAPEAU POUR INCREMENTS VARIABLES
! IN  TAU    : SECOND MEMBRE DE L'EQUATION DE PILOTAGE
! IN  TYPPIL : TYPE PILOTAGE : PRED_ELAS OU DEFORMATION
! OUT NBEFFE : NOMBRE DE SOLUTIONS EFFECTIVES
! OUT ETA    : ETA_PILOTAGE
! OUT PILCVG : CODE DE CONVERGENCE POUR LE PILOTAGE
!                -1 : PAS DE CALCUL DU PILOTAGE
!                 0 : CAS DU FONCTIONNEMENT NORMAL
!                 1 : PAS DE SOLUTION
!                 2 : BORNE ATTEINTE -> FIN DU CALCUL
!
!
!
!
    integer :: nbout, nbin
    parameter    (nbout=1, nbin=23)
    character(len=8) :: lpaout(nbout), lpain(nbin)
    character(len=19) :: lchout(nbout), lchin(nbin)
!
    integer :: nbma, nbpt, icmp, ma, pt, npg, nbgmax
    integer :: jcesd, jcesl,  ja0a1, ja0, ja1, ja2, ja3, jtrav
    integer :: iret, ja4
    real(kind=8) :: result
    character(len=8) :: cpar
    character(len=19) :: copilo, copils, ctau
    character(len=24) :: a0a1, trav
    character(len=19) :: chgeom
    character(len=19) :: depmoi, sigmoi, varmoi, commoi
    character(len=16) :: option
    integer :: ifmdbg, nivdbg
    logical(kind=1) :: debug
    character(len=19) :: xdonco, xindco, lnno, ltno, pinter, ainter, cface
    character(len=19) :: faclon, baseco, xcohes, depplu
    logical(kind=1) :: lcontx
    integer :: ier
    real(kind=8), pointer :: cesv(:) => null()
!
    data copilo, copils  /'&&NMPIPE.COPILO','&&NMPIPE.COPILS'/
    data ctau            /'&&NMPIPE.CTAU'/
    data a0a1, trav      /'&&NMPIPE.A0A1', '&&NMPIPE.TRAV'/
!
! ----------------------------------------------------------------------
!
    call jemarq()
    call infdbg('PRE_CALCUL', ifmdbg, nivdbg)
!
! --- MODELE X-FEM AVEC CONTACT ?
!
    call jeexin(modele(1:8)//'.XFEM_CONT', ier)
    lcontx = ier.ne.0
!
! --- ON FAIT UN CALCUL DE PILOTAGE
!
    pilcvg = 0
!
! --- INITIALISATIONS
!
    if (typpil .eq. 'PRED_ELAS') then
        if (lcontx) then
            option = 'PILO_PRED_XLAS'
        else
            option = 'PILO_PRED_ELAS'
        endif
    else if (typpil.eq.'DEFORMATION') then
        option = 'PILO_PRED_DEFO'
    else
        ASSERT(.false.)
    endif
    debug = nivdbg.ge.2
!
! --- RECUPERATION DES DONNEES XFEM
!
    xindco = resoco(1:14)//'.XFIP'
    xdonco = resoco(1:14)//'.XFDO'
    xcohes = resoco(1:14)//'.XCOH'
    lnno = modele(1:8)//'.LNNO'
    ltno = modele(1:8)//'.LTNO'
    pinter = modele(1:8)//'.TOPOFAC.OE'
    ainter = modele(1:8)//'.TOPOFAC.AI'
    cface = modele(1:8)//'.TOPOFAC.CF'
    faclon = modele(1:8)//'.TOPOFAC.LO'
    baseco = modele(1:8)//'.TOPOFAC.BA'
!
! --- INITIALISATION DES CHAMPS POUR CALCUL
!
    call inical(nbin, lpain, lchin, nbout, lpaout, lchout)
!
! --- DECOMPACTION VARIABLES CHAPEAUX
!
    call nmchex(valinc, 'VALINC', 'DEPMOI', depmoi)
    call nmchex(valinc, 'VALINC', 'SIGMOI', sigmoi)
    call nmchex(valinc, 'VALINC', 'VARMOI', varmoi)
    call nmchex(valinc, 'VALINC', 'COMMOI', commoi)
    call nmchex(valinc, 'VALINC', 'DEPPLU', depplu)
!
    call sdmpic('CHAM_ELEM', sigmoi)
    call sdmpic('CHAM_ELEM', varmoi)
!
! --- CHAMP DE GEOMETRIE
!
    call megeom(modele, chgeom)
!
! --- ALLOCATION DE LA CARTE RESULTAT
!
    call detrsd('CARTE', ctau)
    cpar = 'A0'
    call mecact('V', ctau, 'LIGREL', ligrpi, 'PILO_R',&
                ncmp=1, nomcmp=cpar, sr=tau)
!
! --- REMPLISSAGE DES CHAMPS D'ENTREE
!
    lpain(1) = 'PGEOMER'
    lchin(1) = chgeom
    lpain(2) = 'PMATERC'
    lchin(2) = mate(1:19)
    lpain(3) = 'PCOMPOR'
    lchin(3) = compor(1:19)
    lpain(4) = 'PDEPLMR'
    lchin(4) = depmoi
    lpain(5) = 'PCONTMR'
    lchin(5) = sigmoi
    lpain(6) = 'PVARIMR'
    lchin(6) = varmoi
    lpain(7) = 'PDDEPLR'
    lchin(7) = depdel
    lpain(8) = 'PDEPL0R'
    lchin(8) = ddepl0
    lpain(9) = 'PDEPL1R'
    lchin(9) = ddepl1
    lpain(10)= 'PTYPEPI'
    lchin(10)=  cartyp
    lpain(11)= 'PBORNPI'
    lchin(11)=  careta
    lpain(12)= 'PCDTAU'
    lchin(12)=  ctau
    lpain(13)= 'PCAMASS'
    lchin(13)=  carele(1:8)//'.CARMASSI'
    lpain(14)= 'PINDCOI'
    lchin(14)=  xindco
    lpain(15)= 'PDONCO'
    lchin(15)=  xdonco
    lpain(16) = 'PLSN'
    lchin(16) = lnno
    lpain(17) = 'PLST'
    lchin(17) = ltno
    lpain(18) = 'PPINTER'
    lchin(18) = pinter
    lpain(19) = 'PAINTER'
    lchin(19) = ainter
    lpain(20) = 'PCFACE'
    lchin(20) = cface
    lpain(21) = 'PLONGCO'
    lchin(21) = faclon
    lpain(22) = 'PBASECO'
    lchin(22) = baseco
    lpain(23) = 'PCOHES'
    lchin(23) = xcohes(1:19)
!
! --- REMPLISSAGE DU CHAMP DE SORTIE
!
    lpaout(1) = 'PCOPILO'
    lchout(1) = copilo
!
! --- CALCUL DE L'OPTION
!
    call calcul('S', option, ligrpi, nbin, lchin,&
                lpain, nbout, lchout, lpaout, 'V',&
                'OUI')
!
    if (debug) then
        call dbgcal(option, ifmdbg, nbin, lpain, lchin,&
                    nbout, lpaout, lchout)
    endif
!
! --- EN ATTENDANT DE FAIRE MIEUX, POUR PERMETTRE MUMPS/DISTRIBUE :
!
    call sdmpic('CHAM_ELEM', copilo)
!
! --- TRANSFORMATION EN CHAM_ELEM_S
!
    call celces(copilo, 'V', copils)
    call jeveuo(copils//'.CESD', 'L', jcesd)
    call jeveuo(copils//'.CESL', 'L', jcesl)
    call jeveuo(copils//'.CESV', 'L', vr=cesv)
    nbma = zi(jcesd-1 + 1)
    nbpt = zi(jcesd-1 + 3)
    nbgmax = nbma*nbpt
!
! --- ESPACE MEMOIRE POUR LE TABLEAU A0,A1
!
    call jeexin(a0a1, iret)
    if (iret .eq. 0) then
        call wkvect(a0a1, 'V V R', 4*nbgmax, ja0a1)
        call wkvect(trav, 'V V I', 4*(nbgmax+1), jtrav)
    else
        call jeveuo(a0a1, 'E', ja0a1)
        call jeveuo(trav, 'E', jtrav)
    endif
!
! --- LECTURE DES COMPOSANTES DU CHAM_ELEM_S
!
    icmp = 0
    do ma = 1, nbma
        do pt = 1, nbpt
            call cesexi('C', jcesd, jcesl, ma, pt,&
                        1, 1, ja0)
            call cesexi('C', jcesd, jcesl, ma, pt,&
                        1, 2, ja1)
            call cesexi('C', jcesd, jcesl, ma, pt,&
                        1, 3, ja2)
            call cesexi('C', jcesd, jcesl, ma, pt,&
                        1, 4, ja3)
            call cesexi('C', jcesd, jcesl, ma, pt,&
                        1, 5, ja4)
!
!
!
            if (lcontx) then
! - XFEM : SI PAS DE SOL AU PT DE GAUSS, ON N AJOUTE PAS DE DROITE
                result = abs(&
                         cesv(ja0))+abs(cesv(ja1))+ abs(cesv(ja2))+abs(cesv(1&
                         &-1+ja3)&
                         )
                if (result .eq. 0) then
                    goto 200
                endif
            endif
!
! ---     LECTURE DU CODE RETOUR
!
            if (ja4 .ne. 0) then
                if (cesv(ja4) .ne. r8vide()) then
! ---         A T ON REMPLI CODE-RETOUR ? OUI -> PAS DE SOLUTION
                    pilcvg = 1
                    goto 999
                endif
            endif
!
! ---     COEFFICIENTS DE LA OU DES DROITES
!
            if (ja0.ne.0) then
                if (cesv(ja0).ne.r8vide()) then
                    zr(ja0a1 + icmp    ) = cesv(ja0)
                    zr(ja0a1 + icmp + 1) = cesv(ja1)
                    icmp = icmp+2
                    if (cesv(ja2).ne.r8vide()) then
                        zr(ja0a1 + icmp )    = cesv(ja2)
                        zr(ja0a1 + icmp + 1) = cesv(ja3)
                        icmp = icmp+2
                    endif
                endif
            endif
200         continue
        end do
    end do
!
    npg = icmp / 2
!
! --- RESOLUTION DE L'EQUATION DE PILOTAGE P(U(ETA)) = TAU
!
    call pipere(npg, zr(ja0a1), tau, nbeffe, eta)
!
    if (nbeffe .eq. 0) then
        pilcvg = 1
    endif
!
999  continue
!
    call jedema()
end subroutine
