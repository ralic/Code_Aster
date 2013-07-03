subroutine nmunil(mailla, depplu, ddepla, solveu, matass,&
                  deficu, resocu, cncine, iterat, inst,&
                  ctccvg)
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
! person_in_charge: mickael.abbas at edf.fr
!
    implicit     none
#include "jeveux.h"
#include "asterfort/algocu.h"
#include "asterfort/assert.h"
#include "asterfort/cuprep.h"
#include "asterfort/infniv.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/mtdsc3.h"
    character(len=8) :: mailla
    character(len=19) :: depplu, ddepla, solveu, matass, cncine
    character(len=24) :: deficu
    character(len=24) :: resocu
    integer :: iterat
    real(kind=8) :: inst
    integer :: ctccvg
!
! ----------------------------------------------------------------------
!
! ROUTINE LIAISON_UNILATER
!
! TRAITEMENT DES CONDITIONS UNILATERALES (SAUF CONTACT MECANIQUE)
!
! ----------------------------------------------------------------------
!
!
! IN  DEPPLU : CHAMP DE DEPLACEMENTS A L'ITERATION DE NEWTON PRECEDENTE
! IN  DEPPLA : INCREMENT DE DEPLACEMENTS CALCULE EN IGNORANT LE CONTACT
! IN  SOLVEU : SD SOLVEUR
! IN  DEFICU : SD DE DEFINITION (ISSUE D'AFFE_CHAR_MECA)
! IN  RESOCU : SD DE TRAITEMENT NUMERIQUE
! IN  CNCINE : CHAM_NO CINEMATIQUE
! IN  ITERAT : ITERATION DE NEWTON
! IN  INST   : VALEUR DE L'INSTANT DE CALCUL
! OUT CTCCVG : CODE RETOUR CONTACT DISCRET
!                -1 : PAS DE CALCUL DU CONTACT DISCRET
!                 0 : CAS DU FONCTIONNEMENT NORMAL
!                 1 : NOMBRE MAXI D'ITERATIONS
!                 2 : MATRICE SINGULIERE
!
!
!
!
    character(len=19) :: matr
    integer :: ldscon, lmat
    integer :: ifm, niv, neq
!
! ----------------------------------------------------------------------
!
    call jemarq()
    call infniv(ifm, niv)
!
! --- RECUPERATION DU DESCRIPTEUR DE LA MATRICE DE LIAISON UNIL
!
    matr = resocu(1:14)//'.MATC'
    call mtdsc3(matr)
    call jeveuo(matr(1:19)//'.&INT', 'E', ldscon)
!
! --- INITIALISATIONS
!
    ctccvg = 0
!
! --- RECUPERATION DU DESCRIPTEUR DE LA MATRICE MECANIQUE
!
    call jeveuo(matass//'.&INT', 'E', lmat)
!
! --- PRE-TRAITEMENT DES CONDITIONS UNILATERALES
!
    if (iterat .eq. 0) then
        neq = zi(lmat+2)
        call cuprep(mailla, neq, deficu, resocu, depplu,&
                    inst)
    endif
!
! --- CHOIX DE L'ALGO DE RESOLUTION
!
    call algocu(deficu, resocu, solveu, lmat, ldscon,&
                cncine, ddepla, ctccvg)
!
! --- LE CALCUL DE CONTACT A FORCEMENT ETE REALISE
!
    call assert(ctccvg.ge.0)
!
    call jedema()
end subroutine
