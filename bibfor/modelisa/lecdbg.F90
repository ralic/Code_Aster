subroutine lecdbg(ifl, icl, iv, rv, cv,&
                  cnl, mcl, nbm, nbg, dim,&
                  nob, irteti)
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
!       PREMIERE LECTURE DES DONNEES POUR UN MOT CLE DE TYPE GROUPE
!       ----------------------------------------------------------------
!       IN      IFL,ICL,IV,RV,CV,CNL = VOIR LIRITM
!               MCL             = MOTS CLE TYPE DEBUG
!               NBM             = NB DE MOTS CLES TYPE DEBUG
!                               = 1 > ERREUR EN LECTURE
!               DIM             = NB DE NOMS LUS PAR MOT CLE DEBUG
!               NOB             = NOMS LUS
!               NBG             = NIVEAU DEBUG
!               (RETURN)        = MOT CLE SUIVANT (MOT CLE NON RECONNU)
!               (RETURN 1)      = EXIT            (MOT CLE FIN TROUVE)
!               (RETURN 2)      = LIGNE SUIVANTE  (MOT CLE FINSF TROUVE
!                                                  OU ERREUR DETECTE)
!       ----------------------------------------------------------------
!
#include "asterfort/iunifi.h"
#include "asterfort/liritm.h"
#include "asterfort/tesfin.h"
#include "asterfort/tesmcl.h"
#include "asterfort/utmess.h"
    integer :: nbm
    real(kind=8) :: rv
    character(len=8) :: mcl(nbm)
    integer :: dim(nbm), deblig
    character(len=14) :: cnl
    character(len=16) :: cmd
    character(len=24) :: nob(50, nbm), b24, mtc
    character(len=*) :: cv
    common          /opmail/        cmd
    save b24
!-----------------------------------------------------------------------
    integer :: i, icl, ifl, ifm, irtet, irteti
    integer :: iv, nbg, numtcl
!-----------------------------------------------------------------------
    data b24        /'                        '/
    irteti = 0
!
    ifm = iunifi('MESSAGE')
!
! ----- ITEM = MOT CLE TYPE  DEBUG ?
!
    do i = 1, nbm
        call tesmcl(icl, iv, cv, mcl(i), irtet)
        if (irtet .eq. 1) goto 4
        numtcl = i
        goto 5
  4     continue
    end do
    goto 3
!
! ----- LECTURE DES NOMS D OBJETS A DUMPER ?
!
  5 continue
    write(ifm,*)' ----- LECDBG'
  6 continue
    call liritm(ifl, icl, iv, rv, cv,&
                cnl, deblig, 1)
    write(ifm,*)'       LIRITM : ICL = ',icl,&
     &  ' IV = ',iv,' RV = ',rv,' CV(1:8) = ',cv(1:8),' DEBLIG =',deblig
    if (deblig .eq. 1) then
        call tesfin(icl, iv, cv, irtet)
        if (irtet .eq. 1) then
            goto 1
        else if (irtet .eq. 2) then
            goto 2
        endif
    endif
!
! - MOT CLE DUMP
!
    if (numtcl .eq. 1) then
        if (icl .ne. 4 .and. icl .ne. 3) then
            call utmess('F', 'MODELISA4_78', sk=mcl(1))
        else if (iv.gt.24) then
            call utmess('F', 'MODELISA4_79', sk=mcl(1))
        endif
        dim(1) = dim(1) + 1
        mtc = b24
        mtc(1:iv) = cv(1:iv)
        nob(dim(1),1) = mtc
        goto 6
    endif
!
! - MOT CLE DEBUG
!
    if (numtcl .eq. 2) then
        if (icl .ne. 1 .and. icl .ne. 2) then
            call utmess('F', 'MODELISA4_78', sk=mcl(2))
        endif
        if(icl.eq.1)nbg = iv
        if(icl.eq.2)nbg = nint(rv)
        if(nbg.gt.1)nbg = 1
        write(ifm,*)' ------------ DEBUG NIVEAU ',nbg,' --------------'
        goto 6
    endif
!
  1 continue
    irteti = 1
    goto 999
  2 continue
    irteti = 2
    goto 999
  3 continue
    irteti = 0
    goto 999
!
999 continue
end subroutine
