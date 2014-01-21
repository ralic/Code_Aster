subroutine xmelel(ndim, jmail, jtymai, numae, numam,&
                  imod, iatt, imail, nno)
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
!
    implicit none
#include "jeveux.h"
!
#include "asterfort/assert.h"
#include "asterfort/dismte.h"
#include "asterfort/jenuno.h"
#include "asterfort/jexnum.h"
#include "asterfort/teattr.h"
    integer :: ndim, jmail, jtymai, numae, numam
    integer :: imod, iatt(2), imail(2), nno(2)
!
! ----------------------------------------------------------------------
! ROUTINE APPELLEE PAR : XMLIGR
! ----------------------------------------------------------------------
! ROUTINE SPECIFIQUE A L'APPROCHE <<GRANDS GLISSEMENTS AVEC XFEM>>,
! TRAVAIL EFFECTUE EN COLLABORATION AVEC I.F.P.
! ----------------------------------------------------------------------
! RETOURNE DES INFOS SUR LES ELEMENTS DE CONTACT FORMES ENTRE
! DEUX ELEMENTS DE SURFACE
!
! IN  NDIM   : DIMENSSION DU MAILLAGE
! IN JMAIL   : POINTEUR DES TYPES D'ÉLÉMENTS
! IN JTYMAI  : POINTEUR DES TYPES DE MAILLES
! IN NUMAE   : NUMÉRO DE MAILLE ESCLAVE
! IN NUMAM   : NUMÉRO DE MAILLE MAITRE
! OUT IMOD   : NUMÉRO DE MODÉLISATION
! OUT IATT   : NUMÉRO D'ATTRIBUT ESCLAVE ET MAITRE
! OUT IMAIL  : NUMÉRO DE MAILLE ESCLAVE ET MAITRE
! OUT NNO    : NOMBRE DE NOEUDS ESCLAVE ET MAITRE
!
!
!
!
!
    character(len=16) :: typma, typel
    character(len=16) :: mode(3), attr(7), mail(2, 8)
    character(len=16) :: att, mod
    integer :: nbno(2, 8), k, ibid, ier
!
    data (mode(k),k=1,3) /'C_','D_','3D'/
    data (attr(k),k=1,7) /&
     &      'XHC','XHTC','XTC','XH2C','XH3C','XH4C','ARETE'/
    data (mail(1,k),k=1,8) /'TRIA3','QUAD4','TRIA6','QUAD8',&
     &                         ' ',' ',' ',' '/
    data (mail(2,k),k=1,8) /'TETRA4','PYRAM5','PENTA6','HEXA8',&
     &                         'TETRA10','PYRAM13','PENTA15','HEXA20'/
    data (nbno(1,k),k=1,8) /3,4,6,8,-1,-1,-1,-1/
    data (nbno(2,k),k=1,8) /4,5,6,8,10,13,15,20/
!
! ----------------------------------------------------------------------
!
    imod = 0
    iatt(1) = 0
    iatt(2) = 0
    imail(1) = 0
    imail(2) = 0
!
!
! --- TYPE MAILLE ET ELEMENT ESCLAVE
!
    call jenuno(jexnum('&CATA.TM.NOMTM', zi(jtymai-1+numae)), typma)
    call jenuno(jexnum('&CATA.TE.NOMTE', zi(jmail-1+numae)), typel)
! --- RECUPERATION DE LA MODÉLISATION POUR L'ESCALVE
    call dismte('MODELISATION', typel, ibid, mod, ier)
    do 10 k = 1, 3
        if (mode(k) .eq. mod(1:2)) imod = k
10  end do
    ASSERT(imod.ne.0)
! --- RECUPERATION DES L'ATTRIBUT POUR L'ESCALVE
    call teattr('S', 'XFEM', att, ier, typel=typel)
    do 20 k = 1, 7
        if (att .eq. attr(k)) iatt(1) = k
20  end do
    ASSERT(iatt(1).ne.0)
! --- RECUPERATION DU TYPE DE MAILLE POUR L'ESCALVE
    do 30 k = 1, 8
        if (typma .eq. mail(ndim-1,k)) imail(1) = k
30  end do
    ASSERT(imail(1).ne.0)
    nno(1) = nbno(ndim-1,imail(1))
!
!
! --- TYPE MAILLE ET ELEMENT MAITRE
!
    call jenuno(jexnum('&CATA.TM.NOMTM', zi(jtymai-1+numam)), typma)
    call jenuno(jexnum('&CATA.TE.NOMTE', zi(jmail-1+numam)), typel)
! --- RECUPERATION DE LA MODÉLISATION POUR LE MAITRE
    call dismte('MODELISATION', typel, ibid, mod, ier)
    ASSERT(mod(1:2).eq.mode(imod))
! --- RECUPERATION DES L'ATTRIBUT POUR LE MAITRE
    call teattr('S', 'XFEM', att, ier, typel=typel)
    do 40 k = 1, 7
        if (att .eq. attr(k)) iatt(2) = k
40  end do
    ASSERT(iatt(2).ne.0)
! --- RECUPERATION DU TYPE DE MAILLE
    do 50 k = 1, 8
        if (typma .eq. mail(ndim-1,k)) then
            imail(2) = k
        endif
50  end do
    ASSERT(imail(2).ne.0)
    nno(2) = nbno(ndim-1,imail(2))
!
! ---POUR L'ELEMENT EXCLUSIVEEMENT CRACK-TIP
    if (iatt(2) .eq. 3) then
        ASSERT(iatt(1).eq.3)
        nno(2)=0
    endif
end subroutine
