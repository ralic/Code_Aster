subroutine xexiar(vnbadj, vladj, nadjmx, vnuadj, inoa, nunoa, inob, nunob, numar)
    implicit none
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
    integer, intent(in) :: vnbadj(:)
    integer, intent(in) :: nadjmx
    integer, intent(in) :: vladj(:)
    integer, intent(in) :: vnuadj(:)
    integer, intent(in) :: inoa, inob
    integer, intent(in) :: nunoa, nunob
    integer, intent(out) :: numar
!     ------------------------------------------------------------------
! ======================================================================
! COPYRIGHT (C) 1991 - 2014  EDF R&D                  WWW.CODE-ASTER.ORG
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
!    But : chercher si une arete (a, b) a ete stockee dans le tableau de liste d'ajacence,
!          et renvoyer son numero le cas echeant
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
!              inoa   : indice du premier noeud a dans vnbadj
!              nunoa  : numero absolu du noeud a
!              inob   : indice du noeud b dans vnbadj
!              nunob  : numero absolu du noeud b
!
!    Sortie :
!              numar  : deux valeurs possibles :
!                          - numero de l'arete (a, b), si elle est stockee dans le tableau
!                            de listes d'adjacence
!                          - 0, sinon
!     ------------------------------------------------------------------
!
    integer :: iadj, iadja, iadjb

    call jemarq()

    ! recherche de b dans la liste des adjacents de a
    iadja=0
    do iadj=1, vnbadj(inoa)
       if (vladj(nadjmx*(inoa - 1) + iadj) .eq. nunob) then
          ! ici, on a trouve l'arete cherchee
          iadja=iadj
          exit
       endif
    end do
!
    ! recherche de a dans la liste des adjacents de b
    iadjb=0
    do iadj=1, vnbadj(inob)
       if (vladj(nadjmx*(inob - 1) + iadj) .eq. nunoa) then
          ! ici, on a trouve l'arete cherchee
          iadjb=iadj
          exit
       endif
    end do
!
    ! assertion : deux cas possibles
    !   * a est dans la liste de b et b dans la liste de a
    !   * a n'est pas dans la liste de b et b n'est pas dans
    !     la liste de a
    ASSERT((iadja .ne. 0 .and. iadjb .ne. 0) .or. (iadja .eq. 0 .and. iadjb .eq. 0))
!
    ! initialisation de numar : cas ou l'arete n'est pas stockee
    numar=0

    ! l'arete est deja stockee ssi a est pas dans la liste
    ! d'adjacents de b et b est dans la liste d'edjacents de a
    if (iadja .ne. 0 .and. iadjb .ne. 0) then
!      cas ou  l'arete est deja stockee :
!      recuperation du numero de l'arete
       numar=vnuadj(nadjmx*(inoa - 1) + iadja)
    endif

    call jedema()

end subroutine
