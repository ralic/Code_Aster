subroutine cfpeti(resoco, neq, nbliai, nbliac, llf,&
                  llf1, llf2, rho, llliai, llliac)
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
#include "asterc/r8maem.h"
#include "asterc/r8prem.h"
#include "asterfort/caladu.h"
#include "asterfort/cfelpv.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
    character(len=24) :: resoco
    integer :: neq, nbliai
    integer :: nbliac, llf, llf1, llf2
    real(kind=8) :: rho
    integer :: llliai, llliac
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODES DISCRETES - RESOLUTION)
!
! VERIFICATION SI L'ENSEMBLE DES LIAISONS SUPPOSEES EST TROP PETIT
!
! ----------------------------------------------------------------------
!
! IN  RESOCO : SD DE TRAITEMENT NUMERIQUE DU CONTACT
! IN  NEQ    : NOMBRE D'EQUATIONS
! IN  NBLIAC : NOMBRE DE LIAISONS ACTIVES
! IN  LLF    : NOMBRE DE LIAISONS DE FROTTEMENT (EN 2D)
!              NOMBRE DE LIAISONS DE FROTTEMENT SUIVANT LES DEUX
!               DIRECTIONS SIMULTANEES (EN 3D)
! IN  LLF1   : NOMBRE DE LIAISONS DE FROTTEMENT SUIVANT LA
!               PREMIERE DIRECTION (EN 3D)
! IN  LLF2   : NOMBRE DE LIAISONS DE FROTTEMENT SUIVANT LA
!               SECONDE DIRECTION (EN 3D)
! IN  NBLIAI : NOMBRE DE LIAISONS
! OUT RHO    : COEFFICIENT DE MISE A JOUR
!               1   - TOUTES LES LIAISONS SONT ACTIVES
!               VAL - VALEUR A CORRIGER SUR LE JEU POUR LA LIAISON LLMIN
! OUT LLLIAI : NUMERO DE LA LIAISON LA PLUS VIOLEE
! OUT LLLIAC : NUMERO DE LA LIAISON _ACTIVE_ LA PLUS VIOLEE
!
!
!
!
    real(kind=8) :: un
    parameter    (un=1.d0)
    real(kind=8) :: rhorho
    real(kind=8) :: aadelt, jeuold, jeunew, jeuinc
    logical(kind=1) :: liaiac, delpos, lelpiv
    integer :: btotal, iliai, iliac
    character(len=19) :: liac
    integer :: jliac
    character(len=24) :: apcoef, apddl, appoin
    integer :: japcoe, japddl, japptr
    character(len=24) :: jeuite
    integer :: jjeuit
    character(len=19) :: ddeplc, ddelt
    integer :: nbddl, jdecal
    character(len=2) :: typec0
    real(kind=8), pointer :: vddelt(:) => null()
    real(kind=8), pointer :: ddepc(:) => null()
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- ACCES STRUCTURES DE DONNEES DE CONTACT
!
    liac = resoco(1:14)//'.LIAC'
    appoin = resoco(1:14)//'.APPOIN'
    apcoef = resoco(1:14)//'.APCOEF'
    apddl = resoco(1:14)//'.APDDL'
    jeuite = resoco(1:14)//'.JEUITE'
    call jeveuo(liac, 'L', jliac)
    call jeveuo(appoin, 'L', japptr)
    call jeveuo(apcoef, 'L', japcoe)
    call jeveuo(apddl, 'L', japddl)
    call jeveuo(jeuite, 'L', jjeuit)
!
! --- ACCES AUX CHAMPS DE TRAVAIL
! --- DDEPLC: INCREMENT DE SOLUTION APRES CORRECTION DU CONTACT
! --- DDELT : INCREMENT DE SOLUTION ITERATION DE CONTACT
!
    ddeplc = resoco(1:14)//'.DELC'
    ddelt = resoco(1:14)//'.DDEL'
    call jeveuo(ddeplc(1:19)//'.VALE', 'L', vr=ddepc)
    call jeveuo(ddelt (1:19)//'.VALE', 'L', vr=vddelt)
!
! --- INITIALISATIONS
!
    rhorho = r8maem()
    delpos = .false.
    typec0 = 'C0'
    llliai = 0
    btotal = nbliac+llf+llf1+llf2
!
! --- VERIFICATION : ENSEMBLE DES LIAISONS SUPPOSEES TROP PETIT ?
!
    if (nbliac .eq. nbliai) then
!
! ----- TOUTES LES LIAISONS SONT ACTIVES
!
        rhorho = un
    else if (nbliac.lt.nbliai) then
!
! ----- RECHERCHE DES LIAISONS NON ACTIVES
!
        do 180 iliai = 1, nbliai
!
            liaiac = .false.
!
! ------- LA LIAISON ILIAI EST-ELLE ACTIVE ? (-> LIAIAC)
!
            do 170 iliac = 1, btotal
                if (zi(jliac-1+iliac) .eq. iliai) liaiac = .true.
170          continue
!
! ------- CALCUL DE RHOMIN
!
            if (.not.liaiac) then
!
! --------- CALCUL DE [A].{DDELT} SI LA LIAISON N'EST PAS ACTIVE
!
                jdecal = zi(japptr+iliai-1)
                nbddl = zi(japptr+iliai) - zi(japptr+iliai-1)
                call caladu(neq, nbddl, zr(japcoe+jdecal), zi(japddl+ jdecal), vddelt,&
                            aadelt)
!
! --------- SI [A].{DDELT} EST POSITIF: LIAISON ACTIVEE
!
                if (aadelt .gt. r8prem()) then
!
! ----------- ON NE PREND PAS EN COMPTE UNE LIAISON A PIVOT NUL
!
                    call cfelpv(iliai, typec0, resoco, nbliai, lelpiv)
                    if (lelpiv) then
                        goto 180
                    else
                        delpos = .true.
!
! ------------- VALEUR DE {JEU(DEPTOT)} : JEU AVANT ITERATION DE NEWTON
!
                        jeuold = zr(jjeuit+3*(iliai-1)+1-1)
!
! ------------- CALCUL DE [A].{DDEPLC} - CORRECTION DU JEU
!
                        call caladu(neq, nbddl, zr(japcoe+jdecal), zi(japddl+jdecal), ddepc,&
                                    jeuinc)
!
! ------------- CALCUL DE {JEU(DEPTOT) - [A].{DDEPLC}}/[A].{DDELT}
!
                        jeunew = jeuold-jeuinc
                        jeunew = jeunew/aadelt
!
! ------------- RHOMIN = MIN({JEU(DEPTOT) - A.DDEPLC}/{{A.DDELT})
!
                        if (jeunew .lt. rhorho) then
                            rhorho = jeunew
!
! --------------- LLLIAI: LIAISON LA PLUS VIOLEE
!
                            llliai = iliai
                        endif
                    endif
                endif
            endif
180      continue
!
! ----- TOUS LES {A.DELTA} SONT NEGATIFS
!
        if (.not.delpos) then
            rhorho = un
        endif
    endif
!
! --- ON FAIT EN SORTE QUE RHORHO <= 1.D0
!
    rho = min(rhorho,un)
!
! --- LIAISON ACTIVE
!
    if (llliai .ne. 0) then
        llliac = nbliac + llf + llf1 + llf2 + 1
    endif
!
    call jedema()
!
end subroutine
