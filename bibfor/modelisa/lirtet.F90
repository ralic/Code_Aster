subroutine lirtet(ifl, ilec, inom, cnl, nom,&
                  icl, iv, rv, cv, deblig)
    implicit none
!       ----------------------------------------------------------------
! ======================================================================
! COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
!       BUT: SAUT D'UNE EVENTUELLE ENTETE AVEC LECTURE DE "NOM="
!       IN: IFL : FICHIER OU ON LIT
!          ILEC: 1=> PREMIERE LECTURE, 2=> SECONDE LECTURE
!          INOM: 1=> LECTURE DU NOM,   0=> NOM IGNORE
!          CNL : NUMERO DE LA LIGNE DANS UNE CHAINE
!          DEBLIG : IN ET OUT DANS LIRITM
!       OUT:
!          NOM : ITEM DERRIERE "NOM="
!          ICL, IV, RV, CV, DEBLIG : COMME LIRITM
!       EN SORTIE DE CETTE ROUTINE, ON A LU LA PREMIERE LIGNE
!       D'INFORMATIONS
!       ----------------------------------------------------------------
!
#include "asterfort/assert.h"
#include "asterfort/liritm.h"
#include "asterfort/lirlig.h"
#include "asterfort/u2mesk.h"
#include "asterfort/u2mess.h"
    common/opmail/cmd
    character(len=16) :: cmd
    character(len=14) :: cnl
    character(len=80) :: lig
    character(len=8) :: cvz
    character(len=24) :: nom
    character(len=*) :: cv
    integer :: deblig
    real(kind=8) :: rv
    logical :: lnom, lent
!
!-----------------------------------------------------------------------
    integer :: i, icl, ifl, ilec, inom, iv
    integer :: lcv, nbigno
!-----------------------------------------------------------------------
    lnom=.false.
    lent=.false.
    nbigno=0
    nom='INDEFINI'
!
    ASSERT(inom.eq.0.or.inom.eq.1)
!
    if (inom .eq. 0) then
 1      continue
        cv=' '
        iv=0
        rv=0.d0
        call liritm(ifl, icl, iv, rv, cv(1:8),&
                    cnl, deblig, ilec)
        if (deblig .eq. 0) then
! -     IL Y A UNE ENTETE
            if (icl .eq. 3) then
                if (cv(1:6) .eq. 'NBLIGE') then
                    call liritm(ifl, icl, iv, rv, cv(1:8),&
                                cnl, deblig, ilec)
                    if (icl .eq. 1) then
                        nbigno=iv
                    else
                        ASSERT(.false.)
                    endif
                    goto 9
                else
! -         L'IDENTIFICATEUR LU N'EST PAS "NBLIGE"
                    goto 1
                endif
            else
! -       L'ITEM LU N'EST PAS UN IDENTIFICATEUR
                goto 1
            endif
        else
! -     PAS D'ENTETE
            goto 9
        endif
!
    else if (inom.eq.1) then
 2      continue
        cv=' '
        iv=0
        rv=0.d0
        call liritm(ifl, icl, iv, rv, cv(1:8),&
                    cnl, deblig, ilec)
        if (deblig .eq. 0) then
! -     IL Y A UNE ENTETE
            if (icl .eq. 3) then
! -       L'ITEM LU EST UN IDENTIFICATEUR
                if (cv(1:3) .eq. 'NOM') then
                    call liritm(ifl, icl, iv, rv, cv(1:24),&
                                cnl, deblig, ilec)
                    if (icl .eq. 3) then
                        if (iv .gt. 24) then
                            cvz = cv
                            call u2mesk('A', 'MODELISA4_97', 1, cvz(1:iv))
                        endif
                        lcv=min(iv,24)
                        ASSERT(len(cv).ge.lcv)
                        nom=cv(1:lcv)
                    else
                        call u2mess('F', 'MODELISA4_98')
                    endif
                    lnom=.true.
                else if (cv(1:6).eq.'NBLIGE') then
                    cv=' '
                    iv=0
                    rv=0.d0
                    call liritm(ifl, icl, iv, rv, cv(1:8),&
                                cnl, deblig, ilec)
                    if (icl .eq. 1) then
                        nbigno=iv
                    else
                        ASSERT(.false.)
                    endif
                    lent=.true.
                else
! -          L'IDENTIFICATEUR LU N'EST NI "NBLIGE", NI "NOM"
                    goto 2
                endif
                if (lnom .and. lent) then
                    goto 9
                else
                    goto 2
                endif
            else
! -       L'ITEM LU N'EST PAS UN IDENTIFICATEUR
                goto 2
            endif
        else
            if (lent) then
                ASSERT(.not.lnom)
                nbigno = nbigno - 1
                goto 9
            else
! -       PAS D'ENTETE
                goto 9
            endif
        endif
    endif
!
 9  continue
    if (nbigno .gt. 0) then
        do 99 i = 1, nbigno-1
            call lirlig(ifl, cnl, lig, ilec)
99      continue
        deblig=-1
        call liritm(ifl, icl, iv, rv, cv(1:8),&
                    cnl, deblig, ilec)
    endif
!
end subroutine
