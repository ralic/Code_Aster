subroutine xlagsc(ndim, nbno, nbarto, mxar, algola,&
                  jtabno, jtabin, jtabcr, fiss, nliseq)
!
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
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/wkvect.h"
#include "asterfort/xrell1.h"
#include "asterfort/xrell2.h"
#include "asterfort/xsella.h"
    integer :: jtabno, jtabin, jtabcr
    integer :: nbno, nbarto, ndim, mxar
    integer :: algola
    character(len=19) :: nliseq
    character(len=8) :: fiss
!
! ----------------------------------------------------------------------
!
! ROUTINE XFEM (PREPARATION)
!
! CHOIX DE L'ESPACE DES LAGRANGES POUR LE CONTACT:
!                    (VOIR BOOK VI 15/07/05)
!    - DETERMINATION DES NOEUDS
!    - CREATION DES RELATIONS DE LIAISONS ENTRE LAGRANGE
!
! ----------------------------------------------------------------------
!
!
! IN  MXAR   : NOMBRE MAXIMUM D'ARETES COUPEES PAR LA FISSURE
! IN  JTABNO : ADRESSE DU TABLEAU DES NOEUDS EXTREMITES ET NOEUD MILIEU
! IN  JTABIN : ADRESSE DU TABLEAU DES COORDONNEES DES NOEUDS MILIEU
! IN  NBNO   : NOMBRE DE NOEUDS DU MAILLAGE
! IN  FISS   : NOM DE LA FISSURE
! IN  ALGOLA : TYPE DE CREATION DES RELATIONS DE LIAISONS ENTRE LAGRANGE
! I/O NBARTO : IN   NOMBRE D'ARETES INITIALES
!              OUT  NOMBRE D'ARETES COUPEES
! IN  MXAR   : NOMBRE MAXIMUM D'ARETES COUPEES PAR LA FISSURE
! OUT NLISRL : LISTE REL. LIN. POUR V1 ET V2
! OUT NLISCO : LISTE REL. LIN. POUR V1 ET V2
! OUT NLISEQ : LISTE REL. LIN. POUR V2 SEULEMENT
!
!
!
!
    character(len=19) :: pickno
    integer :: jpino
    integer :: nbpino
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATIONS
!
    pickno = '&&XLAGSP.PICKNO'
!
! --- CREATION DES LISTES DES RELATIONS DE LIAISONS ENTRE LAGRANGE
!
    if (algola .eq. 1) then
!
! --- CREATION TABLEAU DES NOEUDS EXTREMITES ET NOEUD MILIEU
!
        call wkvect(pickno, 'V V I', mxar, jpino)
!
! --- SELECTION DES NOEUDS
!
        call xsella(fiss, nbno, nbarto, zi(jtabno), zi(jpino),&
                    nbpino)
!
! --- CREATION DES LISTES DES RELATIONS DE LIAISONS ENTRE LAGRANGE
!
        call xrell1(zi(jtabno), ndim, nbarto, zi(jpino), nbpino,&
                    nliseq)
!
! --- MENAGE
!
        call jedetr(pickno)
!
    else if (algola.eq.2) then
!
! --- CREATION DES LISTES DES RELATIONS DE LIAISONS ENTRE LAGRANGE
!
        call xrell2(zi(jtabno), ndim, nbarto, zr(jtabin), zr(jtabcr),&
                    .false., nliseq)
    endif
!
    call jedema()
end subroutine
