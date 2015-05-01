subroutine nmcret(sderro, typcod, vali)
!
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
! person_in_charge: mickael.abbas at edf.fr
!
    implicit     none
#include "jeveux.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/nmcrel.h"
    character(len=24) :: sderro
    character(len=3) :: typcod
    integer :: vali
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (SD ERREUR)
!
! GENERE UN EVENEMENT A PARTIR D'UN CODE RETOUR
!
! ----------------------------------------------------------------------
!
!
! IN  SDERRO : SD GESTION DES ERREURS
! IN  TYPCOD : TYPE DU CODE RETOUR
!             'LDC' - INTEG. DU COMPORTEMENT
!                -1 : PAS D'INTEGRATION DU COMPORTEMENT
!                 0 : CAS DU FONCTIONNEMENT NORMAL
!                 1 : ECHEC DE L'INTEGRATION DE LA LDC
!                 2 : ERREUR SUR LA NON VERIF. DE CRITERES PHYSIQUES
!                 3 : SIZZ PAS NUL POUR C_PLAN DEBORST
!             'PIL' - PILOTAGE
!                -1 : PAS DE CALCUL DU PILOTAGE
!                 0 : CAS DU FONCTIONNEMENT NORMAL
!                 1 : PAS DE SOLUTION
!                 2 : BORNE ATTEINTE -> FIN DU CALCUL
!             'FAC' - FACTORISATION
!                -1 : PAS DE FACTORISATION
!                 0 : CAS DU FONCTIONNEMENT NORMAL
!                 1 : MATRICE SINGULIERE
!                 2 : ERREUR LORS DE LA FACTORISATION
!                 3 : ON NE SAIT PAS SI SINGULIERE
!             'RES' - RESOLUTION
!                -1 : PAS DE RESOLUTION
!                 0 : CAS DU FONCTIONNEMENT NORMAL
!                 1 : NOMBRE MAXIMUM D'ITERATIONS ATTEINT
!             'CTC' - CONTACT DISCRET
!                -1 : PAS DE CALCUL DU CONTACT DISCRET
!                 0 : CAS DU FONCTIONNEMENT NORMAL
!                 1 : NOMBRE MAXI D'ITERATIONS
!                 2 : MATRICE SINGULIERE
! IN  VALI   : VALEUR DU CODE RETOUR
!
!
!
!
    integer :: ieven, zeven
    character(len=24) :: errecn, errecv, erreno
    integer :: jeecon, jeecov, jeenom
    character(len=24) :: errinf
    integer :: jeinf
    integer :: vcret
    character(len=8) :: ncret
    character(len=9) :: neven
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- L'OPERATION N'A PAS EU LIEU -> ON SORT
!
    if (vali .eq. -1) goto 999
!
! --- ACCES SD
!
    errinf = sderro(1:19)//'.INFO'
    call jeveuo(errinf, 'L', jeinf)
    zeven = zi(jeinf-1+1)
!
    erreno = sderro(1:19)//'.ENOM'
    errecv = sderro(1:19)//'.ECOV'
    errecn = sderro(1:19)//'.ECON'
    call jeveuo(erreno, 'L', jeenom)
    call jeveuo(errecv, 'L', jeecov)
    call jeveuo(errecn, 'L', jeecon)
!
! --- RECHERCHE EVENEMENT ATTACHE A CE CODE-RETOUR
!
    do 15 ieven = 1, zeven
!
! ----- NOM DE L'EVENEMENT
!
        neven = zk16(jeenom-1+ieven)(1:9)
!
! ----- NOM DU CODE RETOUR
!
        ncret = zk8 (jeecon-1+ieven)
!
! ----- VALEUR DU CODE RETOUR
!
        vcret = zi (jeecov-1+ieven)
!
! ----- ACTIVATION DE L'EVENEMENT
!
        if (ncret .eq. typcod) then
            if (vcret .eq. vali) then
                call nmcrel(sderro, neven, .true._1)
            else
                call nmcrel(sderro, neven, .false._1)
            endif
        endif
15  end do
!
999  continue
!
    call jedema()
end subroutine
