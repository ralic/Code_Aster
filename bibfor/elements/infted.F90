subroutine infted(nomte, symetr, nbterm, nbnoeu, nbcomp,&
                  ndimen, itype)
    implicit none
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/poutre_modloc.h"
#include "asterfort/tecael.h"
#include "asterfort/utmess.h"
!
    character(len=*) :: nomte
    integer :: symetr, nbterm, nbnoeu, nbcomp, ndimen, itype
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
! person_in_charge: jean-luc.flejou at edf.fr
! --- ------------------------------------------------------------------
!                 INFORMATIONS SUR LES DISCRETS ET POUTRES
! IN
!     NOMTE : ELEMENTS CONCERNES :
!        MECA_DIS_TR_L     : MAILLE A 2 NOEUDS EN 3D
!        MECA_DIS_T_L      : MAILLE A 2 NOEUDS EN 3D
!        MECA_DIS_TR_N     : MAILLE A 1 NOEUD  EN 3D
!        MECA_DIS_T_N      : MAILLE A 1 NOEUD  EN 3D
!        MECA_2D_DIS_TR_L  : MAILLE A 2 NOEUDS EN 2D
!        MECA_2D_DIS_T_L   : MAILLE A 2 NOEUDS EN 2D
!        MECA_2D_DIS_TR_N  : MAILLE A 1 NOEUD  EN 2D
!        MECA_2D_DIS_T_N   : MAILLE A 1 NOEUD  EN 2D
!        MECA_POU_D_T      : MAILLE A 2 NOEUDS EN 3D
!        MECA_POU_D_E      : MAILLE A 2 NOEUDS EN 3D
!        MECA_POU_D_EM     : MAILLE A 2 NOEUDS EN 3D
!        MECA_POU_C_T      : MAILLE A 2 NOEUDS EN 3D
!        MECA_POU_D_TG     : MAILLE A 2 NOEUDS EN 3D
!        MECA_POU_D_TGM    : MAILLE A 2 NOEUDS EN 3D
!     SYMETR : =1 NON-SYMETRIQUE, =2 NON-SYMETRIQUE (DISCRETS SEULEMENT)
!
! OUT
!     NBTERM : NOMBRE DE TERME DANS LA MATRICE
!     NBNOEU : NOMBRE DE NOEUDS DE L'ELEMENT
!     NBCOMP : NOMBRE DE COMPOSANTE PAR NOEUD
!     NDIMEN : DIMENSION DE L'ELEMENT
!     ITYPE  : TYPE DE L'ELEMENT
!
!     CHARACTER*32 JEXNUM,JEXNOM,JEXR8,JEXATR
!
    character(len=20) :: kmess(5)
    integer :: iadzi, iazk24, iret
    real(kind=8) :: tvar
! --- ------------------------------------------------------------------
!
!     ITYPE : DANS PTENPO
!        POUTRE DROITE DE SECTION CONSTANTE OU VARIABLE  : 0 1 2
!        POUTRE COURBE DE SECTION CONSTANTE              : 10
!
!                                                  T  TR
!        DISCRET TYPE NODALE  ..._N              : 20 21
!        DISCRET TYPE NODALE  ..._N_NS           : 22 23
!        DISCRET TYPE LIAISON ..._L              : 40 41
!        DISCRET TYPE LIAISON ..._L_NS           : 42 43
!
    itype = -10
    if (nomte .eq. 'MECA_DIS_TR_L') then
        ASSERT((symetr.eq.1).or.(symetr.eq.2))
        if (symetr .eq. 1) then
            nbterm = 78
            nbnoeu = 2
            nbcomp = 6
            ndimen = 3
            itype = 41
        else if (symetr.eq.2) then
            nbterm = 144
            nbnoeu = 2
            nbcomp = 6
            ndimen = 3
            itype = 43
        endif
    else if (nomte.eq.'MECA_DIS_TR_N') then
        ASSERT((symetr.eq.1).or.(symetr.eq.2))
        if (symetr .eq. 1) then
            nbterm = 21
            nbnoeu = 1
            nbcomp = 6
            ndimen = 3
            itype = 21
        else if (symetr.eq.2) then
            nbterm = 36
            nbnoeu = 1
            nbcomp = 6
            ndimen = 3
            itype = 23
        endif
    else if (nomte.eq.'MECA_DIS_T_L') then
        ASSERT((symetr.eq.1).or.(symetr.eq.2))
        if (symetr .eq. 1) then
            nbterm = 21
            nbnoeu = 2
            nbcomp = 3
            ndimen = 3
            itype = 40
        else if (symetr.eq.2) then
            nbterm = 36
            nbnoeu = 2
            nbcomp = 3
            ndimen = 3
            itype = 42
        endif
    else if (nomte.eq.'MECA_DIS_T_N') then
        ASSERT((symetr.eq.1).or.(symetr.eq.2))
        if (symetr .eq. 1) then
            nbterm = 6
            nbnoeu = 1
            nbcomp = 3
            ndimen = 3
            itype = 20
        else if (symetr.eq.2) then
            nbterm = 9
            nbnoeu = 1
            nbcomp = 3
            ndimen = 3
            itype = 22
        endif
    else if (nomte.eq.'MECA_2D_DIS_TR_L') then
        ASSERT((symetr.eq.1).or.(symetr.eq.2))
        if (symetr .eq. 1) then
            nbterm = 21
            nbnoeu = 2
            nbcomp = 3
            ndimen = 2
            itype = 41
        else if (symetr.eq.2) then
            nbterm = 36
            nbnoeu = 2
            nbcomp = 3
            ndimen = 2
            itype = 43
        endif
    else if (nomte.eq.'MECA_2D_DIS_TR_N') then
        ASSERT((symetr.eq.1).or.(symetr.eq.2))
        if (symetr .eq. 1) then
            nbterm = 6
            nbnoeu = 1
            nbcomp = 3
            ndimen = 2
            itype = 21
        else if (symetr.eq.2) then
            nbterm = 9
            nbnoeu = 1
            nbcomp = 3
            ndimen = 2
            itype = 23
        endif
    else if (nomte.eq.'MECA_2D_DIS_T_L') then
        ASSERT((symetr.eq.1).or.(symetr.eq.2))
        if (symetr .eq. 1) then
            nbterm = 10
            nbnoeu = 2
            nbcomp = 2
            ndimen = 2
            itype = 40
        else if (symetr.eq.2) then
            nbterm = 16
            nbnoeu = 2
            nbcomp = 2
            ndimen = 2
            itype = 42
        endif
    else if (nomte.eq.'MECA_2D_DIS_T_N') then
        ASSERT((symetr.eq.1).or.(symetr.eq.2))
        if (symetr .eq. 1) then
            nbterm = 3
            nbnoeu = 1
            nbcomp = 2
            ndimen = 2
            itype = 20
        else if (symetr.eq.2) then
            nbterm = 4
            nbnoeu = 1
            nbcomp = 2
            ndimen = 2
            itype = 22
        endif
!
!     LES POUTRES
    else if (nomte.eq.'MECA_POU_D_T') then
        nbterm = 78
        nbnoeu = 2
        nbcomp = 6
        ndimen = 3
        call poutre_modloc('CAGNPO', ['TVAR'], 1, valeur=tvar, arret='NNN', retour= iret)
        if (iret .eq. 0) itype = nint(tvar)
    else if (nomte.eq.'MECA_POU_D_E') then
        nbterm = 78
        nbnoeu = 2
        nbcomp = 6
        ndimen = 3
        call poutre_modloc('CAGNPO', ['TVAR'], 1, valeur=tvar, arret='NNN', retour= iret)
        if (iret .eq. 0) itype = nint(tvar)
    else if (nomte.eq.'MECA_POU_D_EM') then
        nbterm = 78
        nbnoeu = 2
        nbcomp = 6
        ndimen = 3
        call poutre_modloc('CAGNPO', ['TVAR'], 1, valeur=tvar, arret='NNN', retour= iret)
        if (iret .eq. 0) itype = nint(tvar)
    else if (nomte.eq.'MECA_POU_C_T') then
        nbterm = 78
        nbnoeu = 2
        nbcomp = 6
        ndimen = 3
        call poutre_modloc('CAGNPO', ['TVAR'], 1, valeur=tvar, arret='NNN', retour= iret)
        if (iret .eq. 0) itype = nint(tvar)
    else if ( (nomte.eq.'MECA_POU_D_TG') .or. (&
               nomte.eq.'MECA_POU_D_TGM') ) then
        nbterm = 105
        nbnoeu = 2
        nbcomp = 7
        ndimen = 3
        call poutre_modloc('CAGNPO', ['TVAR'], 1, valeur=tvar, arret='NNN', retour= iret)
        if (iret .eq. 0) itype = nint(tvar)
!
!     L'ELEMENT N'EST PAS TRAITE
    else
        kmess(1) = nomte
        kmess(2) = 'INFTED'
        call tecael(iadzi, iazk24)
        kmess(3) = zk24(iazk24-1+3)
        call utmess('F', 'DISCRETS_13', nk=3, valk=kmess)
    endif
!
end subroutine
