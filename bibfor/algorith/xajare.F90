subroutine xajare(vnbadj, vladj, nadjmx, vnuadj, inoa, nunoa, inob, nunob, nbedge)
    implicit none
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
    integer, intent(inout) :: vnbadj(:)
    integer, intent(in) :: nadjmx
    integer, intent(inout) :: vladj(:)
    integer, intent(inout) :: vnuadj(:)
    integer, intent(in) :: inoa, inob
    integer, intent(in) :: nunoa, nunob
    integer, intent(inout) :: nbedge
!     ------------------------------------------------------------------
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
! person_in_charge: patrick.massin at edf.fr
!
!    But : stocker l'arete 'a, b) dans le tableau de liste d'ajacence,
!          et renvoyer son numero
!
!    Entrees :
!              vnbadj : nombre de noeuds adjacents a chaque noeud
!              nadjmx : nombre maximum de noeuds adjacents a un noeud
!              vladj  : liste des noeuds adjacents a chaque noeud
!                       la liste de noeuds adjacents au noeud d'indice ino
!                       est de longueur vnbadj(ino) et elle stockee dans :
!                         vladj(nadjmx*(ino - 1) + 1:nadjmx*(ino - 1) + vnbadj(ino))
!                       N.B.: vladj(:) stocke des numeros de noeuds absolus
!              vnuadj : stockage des numeros d'aretes stockes dans les listes d'adjacence
!              nbedge : nombre d'arete stockees dans le tableau de listes d'adjacence
!              inoa   : indice du premier noeud a dans vnbadj
!              nunoa  : numero absolu du noeud a
!              inob   : indice du noeud b dans vnbadj
!              nunob  : numero absolu du noeud b
!
!    Sortie :
!              nbedge : le nombre d'arete stockee est incrementee
!     ------------------------------------------------------------------
!
    call jemarq()

    ! stockage de b dans la liste des adjacents de a
    vnbadj(inoa) = vnbadj(inoa) + 1
!
    ! verification du nombre d'adjacents stockes
    ! dans la liste des adjacents de a
    ASSERT(vnbadj(inoa) .le. nadjmx)
!
    vladj(nadjmx*(inoa - 1) + vnbadj(inoa)) = nunob
!
    ! stockage de a dans la liste des adjacents de b
    vnbadj(inob) = vnbadj(inob) + 1
!
    ! verification du nombre d'adjacents stockes
    ! dans la liste des adjacents de b
    ASSERT(vnbadj(inob) .le. nadjmx)
!
    vladj(nadjmx*(inob - 1) + vnbadj(inob)) = nunoa
!
    ! incrementation du nombre d'aretes stockees
    nbedge = nbedge + 1

    ! numerotation de l'arete
    vnuadj(nadjmx*(inoa - 1) + vnbadj(inoa)) = nbedge
    vnuadj(nadjmx*(inob - 1) + vnbadj(inob)) = nbedge

    call jedema()

end subroutine
