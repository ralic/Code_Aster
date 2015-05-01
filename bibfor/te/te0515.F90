subroutine te0515(option, nomte)
    implicit none
#include "asterf_types.h"
#include "jeveux.h"
#include "asterc/ismaem.h"
#include "asterfort/assert.h"
#include "asterfort/assesu.h"
#include "asterfort/caethm.h"
#include "asterfort/fnoesu.h"
#include "asterfort/jevech.h"
    character(len=16) :: option, nomte
! =====================================================================
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
! =====================================================================
!    - FONCTION REALISEE:  CALCUL DES OPTIONS NON-LINEAIRES MECANIQUES
!                          ELEMENTS HH2_SUDA, (SUSHI DECENTRE ARRETE)
!    - ARGUMENTS:
!        DONNEES:      OPTION       -->  OPTION DE CALCUL
!                      NOMTE        -->  NOM DU TYPE ELEMENT
! =====================================================================
    integer :: jgano, nno, imatuu, ndim, imate, iinstm, jcret
    integer :: ipoid2, ivf2
    integer :: idfde2, npi, npg, npi2
!
    integer :: retloi
    integer :: ipoids, ivf, idfde, igeom
    integer :: iinstp, ideplm, ideplp, icompo, icarcr
    integer :: icontm, ivarip, ivarim, ivectu, icontp
! =====================================================================
    integer :: mecani(5), press1(7), press2(7), tempe(5), dimuel
    integer :: dimdep, dimdef, dimcon, nbvari, nddls, nddlm, nddlfa, nddlk
    integer :: nmec, np1, np2, nnos
    integer :: nnom, nface
!     REMARQUE : CES DIMENSIONS DOIVENT ETRE LES MEMES QUE DANS TE0492
    real(kind=8) :: defgep(21), defgem(21)
    integer :: nconma, ndefma
    parameter (nconma=31,ndefma=21)
    real(kind=8) :: dsde(nconma, ndefma)
    character(len=3) :: modint
    character(len=8) :: typmod(2)
! =====================================================================
    integer :: li
    aster_logical :: axi, perman, vf
    integer :: typvf
! =====================================================================
!  CETTE ROUTINE FAIT UN CALCUL EN HH2SUDA OU HH2SUC, (HYDRO NON SATURE
!   SUSHI DECENTRE ARETE OU CENTRE)
! =====================================================================
!  POUR LES TABLEAUX DEFGEP ET DEFGEM ON A DANS L'ORDRE :
!                                      PRE1 P1DX P1DY P1DZ
!                                      PRE2 P2DX P2DY P2DZ
! =====================================================================
!    POUR LES CHAMPS DE CONTRAINTE
!                                      M11 FH11X FH11Y FH11Z
!                                      ENT11
!                                      M12 FH12X FH12Y FH12Z
!                                      ENT12
!                                      M21 FH21X FH21Y FH21Z
!                                      ENT21
!                                      M22 FH22X FH22Y FH22Z
!                                      ENT22
! TYPMOD    MODELISATION (D_PLAN, 3D )
! MODINT    METHODE D'INTEGRATION (CLASSIQUE,LUMPEE(D),REDUITE(R) ?)
! NNO       NB DE NOEUDS DE L'ELEMENT
! NNOS      NB DE NOEUDS SOMMETS DE L'ELEMENT
! OUT
!     NFACE  NB DE FACES AU SENS BORD DE DIM DIM-1 NE SERT QU EN VF
!     NNOM   NB DE NOEUDS MILIEUX DE FACE OU D ARRETE NE SERT QU EN EF
!
! NDDLS     NB DE DDL SUR LES SOMMETS
! OUT
!    NDDLM     NB DDL SUR LES MILIEUX DE FACE OU D ARRETE
!              NE SERT QU EN EF
!    NDDLFA    NB DDL SUR LES FACE DE DIMENSION DIM-1 NE SERT QU EN VF
! NDDLK     NB DDL AU CENTRE ELEMENT
! NPI       NB DE POINTS D'INTEGRATION DE L'ELEMENT
! NPG       NB DE POINTS DE GAUSS     POUR CLASSIQUE(=NPI)
!                 SOMMETS             POUR LUMPEE   (=NPI=NNOS)
!                 POINTS DE GAUSS     POUR REDUITE  (<NPI)
! NDIM      DIMENSION DE L'ESPACE
! DIMUEL    NB DE DDL TOTAL DE L'ELEMENT
! DIMCON    DIMENSION DES CONTRAINTES GENERALISEES ELEMENTAIRES
! DIMDEF    DIMENSION DES DEFORMATIONS GENERALISEES ELEMENTAIRES
! IVF       FONCTIONS DE FORMES QUADRATIQUES
! IVF2      FONCTIONS DE FORMES LINEAIRES
! TYPVF     : 3 OU 4 SCHEMA SUSHI DECENTRE ARETE OU CENTRE
! =====================================================================
! =====================================================================
! --- 1. INITIALISATIONS ----------------------------------------------
! --- SUIVANT ELEMENT, DEFINITION DES CARACTERISTIQUES : --------------
! --- CHOIX DU TYPE D'INTEGRATION -------------------------------------
! --- RECUPERATION DE LA GEOMETRIE ET POIDS DES POINTS D'INTEGRATION --
! --- RECUPERATION DES FONCTIONS DE FORME -----------------------------
! =====================================================================
    call caethm(nomte, axi, perman, vf, typvf,&
                typmod, modint, mecani, press1, press2,&
                tempe, dimdep, dimdef, dimcon, nmec,&
                np1, np2, ndim, nno, nnos,&
                nnom, nface, npi, npg, nddls,&
                nddlm, nddlfa, nddlk, dimuel, ipoids,&
                ivf, idfde, ipoid2, ivf2, idfde2,&
                npi2, jgano)
    ASSERT(vf)
! =====================================================================
! --- DEBUT DES DIFFERENTES OPTIONS -----------------------------------
! =====================================================================
! --- 2. OPTIONS : RIGI_MECA_TANG , FULL_MECA , RAPH_MECA -------------
! =====================================================================
    if ((option(1:14).eq.'RIGI_MECA_TANG' ) .or. (option(1:9).eq.'RAPH_MECA' ) .or.&
        (option(1:9).eq.'FULL_MECA' )) then
! =====================================================================
! --- PARAMETRES EN ENTREE --------------------------------------------
! =====================================================================
        call jevech('PGEOMER', 'L', igeom)
        call jevech('PMATERC', 'L', imate)
        call jevech('PINSTMR', 'L', iinstm)
        call jevech('PINSTPR', 'L', iinstp)
        call jevech('PDEPLMR', 'L', ideplm)
        call jevech('PDEPLPR', 'L', ideplp)
        call jevech('PCOMPOR', 'L', icompo)
        call jevech('PCARCRI', 'L', icarcr)
        call jevech('PVARIMR', 'L', ivarim)
        call jevech('PCONTMR', 'L', icontm)
        read (zk16(icompo-1+2),'(I16)') nbvari
! =====================================================================
! --- PARAMETRES EN SORTIE ISMAEM? ------------------------------------
! =====================================================================
        if ((option(1:14).eq.'RIGI_MECA_TANG' ) .or. option(1:9) .eq. 'FULL_MECA') then
            call jevech('PMATUNS', 'E', imatuu)
        else
            imatuu = ismaem()
        endif
        if (option(1:9) .eq. 'RAPH_MECA' .or. option(1:9) .eq. 'FULL_MECA') then
            call jevech('PVECTUR', 'E', ivectu)
            call jevech('PCONTPR', 'E', icontp)
            call jevech('PVARIPR', 'E', ivarip)
            call jevech('PCODRET', 'E', jcret)
            zi(jcret) = 0
        else
            ivectu = ismaem()
            icontp = ismaem()
            ivarip = ismaem()
        endif
        retloi = 0
        if (option(1:14) .eq. 'RIGI_MECA_TANG') then
            call assesu(nno, nnos, nface, zr(igeom), zr(icarcr),&
                        zr( ideplm), zr(ideplm), zr(icontm), zr(icontm), zr(ivarim),&
                        zr( ivarim), defgem, defgem, dsde, zr(imatuu),&
                        zr(ivectu), zr(iinstm), zr(iinstp), option, zi(imate),&
                        mecani, press1, press2, tempe, dimdef,&
                        dimcon, dimuel, nbvari, ndim, zk16( icompo),&
                        typmod, typvf, axi, perman)
        else
!
!   DU FAIT DE L UTIISATION DES VOISINS CETTE BOUCLE
!  NE PEUT PLUS ETRE FAITECONTRAIREMENT A LA SITUATION EF
!  ASSESU UTILISE DELTAP ET PM
            do 30 li = 1, dimuel
                zr(ideplp+li-1) = zr(ideplm+li-1) + zr(ideplp+li-1)
 30         continue
            call assesu(nno, nnos, nface, zr(igeom), zr(icarcr),&
                        zr( ideplm), zr(ideplp), zr(icontm), zr(icontp), zr(ivarim),&
                        zr( ivarip), defgem, defgep, dsde, zr(imatuu),&
                        zr(ivectu), zr(iinstm), zr(iinstp), option, zi(imate),&
                        mecani, press1, press2, tempe, dimdef,&
                        dimcon, dimuel, nbvari, ndim, zk16( icompo),&
                        typmod, typvf, axi, perman)
            zi(jcret) = retloi
        endif
    endif
! ======================================================================
! --- 6. OPTION : FORC_NODA --------------------------------------------
! ======================================================================
    if (option .eq. 'FORC_NODA') then
! ======================================================================
! --- PARAMETRES EN ENTREE ---------------------------------------------
! ======================================================================
        call jevech('PCONTMR', 'L', icontm)
! ======================================================================
! --- PARAMETRES EN SORTIE ---------------------------------------------
! ======================================================================
        call jevech('PVECTUR', 'E', ivectu)
        call fnoesu(option, nno, nnos, nnom, nface,&
                    zr(icontm), zr(ivectu), mecani, press1, press2,&
                    tempe, dimcon, dimuel, typvf, axi,&
                    ipoids, ivf, idfde, ipoid2, ivf2,&
                    idfde2, npi2, jgano, retloi)
    endif
! ======================================================================
end subroutine
