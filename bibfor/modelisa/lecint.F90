subroutine lecint(ifl, icl, iv, rv, cv,&
                  cnl, mcl, nbm, nbg, ier,&
                  irteti)
    implicit none
!       ----------------------------------------------------------------
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!       ----------------------------------------------------------------
!       PREMIERE LECTURE POUR UN MOT CLE INTERFACE OU NON RECONNU
!       ----------------------------------------------------------------
!       IN      IFL,ICL,IV,RV,CV,CNL = VOIR LIRITM
!               MCL             = MOTS CLE TYPE INTERFACE
!               NBG             = NIVEAU DEBUG ASSISTANCE
!               NBM             = NB DE MOTS CLES TYPE INTERFACE
!       OUT     IER             = 0 > LECTURE CORRECTE
!                               = 1 > ERREUR EN LECTURE
!               (RETURN 1)      = EXIT            (MOT CLE FIN TROUVE)
!               (RETURN 2)      = LIGNE SUIVANTE  (MOT CLE FINSF TROUVE
!                                                  OU ERREUR DETECTE)
!       ----------------------------------------------------------------
!
#include "asterfort/codent.h"
#include "asterfort/iunifi.h"
#include "asterfort/liritm.h"
#include "asterfort/tesfin.h"
#include "asterfort/tesmcl.h"
#include "asterfort/utmess.h"
    integer :: nbm
    real(kind=8) :: rv
    character(len=8) :: mcl(nbm)
    integer :: deblig
    character(len=14) :: cnl
    character(len=16) :: cmd, nom
    character(len=24) :: valk(2)
    common          /opmail/        cmd
    character(len=*) :: cv
!
!-----------------------------------------------------------------------
    integer :: i, icl, ier, ifl, ifm, irtet, irteti
    integer :: iv, nbg
!-----------------------------------------------------------------------
    irteti = 0
    ifm = iunifi('MESSAGE')
    if (nbg .ge. 1) write(ifm,*)' ----- LECINT'
!
! -     ITEM = MOT CLE INTERFACE OU AUTRE ?
!
    do i = 1, nbm
        call tesmcl(icl, iv, cv, mcl(i), irtet)
        if (irtet .eq. 1) goto 4
        goto 6
  4     continue
    end do
!
! -     MOT CLE NON RECONNU
!
    if (icl .eq. 1) call codent(iv, 'G', nom)
    if (icl .eq. 2) write(nom,'(F14.6)')rv
    if(icl.eq.3.or.icl.eq.4)nom = cv(1:iv)
    valk(1) = cnl
    valk(2) = nom
    call utmess('E', 'MODELISA4_81', nk=2, valk=valk)
    ier = 1
    goto 5
!
! -     MOT CLE INTERFACE RECONNU
!       ON SORT PAR FIN OU FINSF OU FIN DE FICHIER
!
  6 continue
    nom = cv(1:iv)
    valk(1) = cnl
    valk(2) = nom
    call utmess('I', 'MODELISA4_82', nk=2, valk=valk)
!
  5 continue
    deblig = -1
    call liritm(ifl, icl, iv, rv, cv,&
                cnl, deblig, 1)
    if (nbg .ge. 1) write(ifm, *)'       LIRITM : ICL = ', icl, ' IV = ', iv, ' RV = ', rv,&
                    ' CV(1:8) = ', cv(1:8), ' DEBLIG =', deblig
!
! -     ITEM = MOT  CLE FIN  OU FINSF ?
!
    call tesfin(icl, iv, cv, irtet)
    if (irtet .eq. 1) then
        goto 1
    else if (irtet .eq. 2) then
        goto 2
    endif
!
    goto 5
!
  1 continue
    irteti = 1
    goto 999
  2 continue
    irteti = 2
    goto 999
!
999 continue
end subroutine
