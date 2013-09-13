subroutine dylech(nomo, lischa, nbexre, exreco, exresu)
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
#include "asterc/getfac.h"
#include "asterfort/getvc8.h"
#include "asterfort/getvid.h"
#include "asterfort/infniv.h"
#include "asterfort/lischk.h"
#include "asterfort/lisimp.h"
#include "asterfort/lislec.h"
#include "asterfort/lisnbg.h"
#include "asterfort/lisnnb.h"
#include "asterfort/u2mess.h"
#include "asterfort/wkvect.h"
    character(len=8) :: nomo
    character(len=19) :: lischa
    character(len=24) :: exreco, exresu
    integer :: nbexre
!
! ----------------------------------------------------------------------
!
! DYNA_LINE_HARM
!
! LECTURE DES CHARGEMENTS
!
! ----------------------------------------------------------------------
!
!
! IN  NOMO   : NOM DU MODELE
! OUT LISCHA : SD LISTE DES CHARGES
! OUT NBEXRE : NOMBRE DE EXCIT_RESU
! OUT EXRECO : LISTE DES COEFFICIENTS DANS EXCIT_RESU
! OUT EXRESU : LISTE DES RESULTATS DANS EXCIT_RESU
!
! ----------------------------------------------------------------------
!
    character(len=16) :: motfac, nomcmd
    integer :: iresu, jlccre, jlresu, n
    integer :: nbveas, nbveag, nbveat, nbchar
    integer :: ifm, niv
!
! ----------------------------------------------------------------------
!
    motfac = 'EXCIT'
    nomcmd = 'DYNA_LINE_HARM'
    nbexre = 0
    call infniv(ifm, niv)
!
! --- LECTURE DONNNES CHARGEMENTS
!
    call lislec(motfac, 'MECANIQUE', 'V', lischa)
!
! --- AFFICHAGE DE LA LISTE DES CHARGES
!
    if (niv .ge. 2) call lisimp(lischa, ifm)
!
! --- VERIFICATIONS DE LA LISTE DES CHARGES
!
    call lischk(nomo, 'MECANIQUE', nomcmd, lischa)
!
! --- LECTURE INFORMATIONS EXCIT_RESU
!
    call getfac('EXCIT_RESU', nbexre)
    exreco = '&&DYLECH.COEF_CRE'
    exresu = '&&DYLECH.LISTRESU'
    if (nbexre .ne. 0) then
        call wkvect(exreco, 'V V C  ', nbexre, jlccre)
        call wkvect(exresu, 'V V K8 ', nbexre, jlresu)
        do 252 iresu = 1, nbexre
            call getvid('EXCIT_RESU', 'RESULTAT', iocc=iresu, scal=zk8( jlresu+iresu-1), nbret=n)
            call getvc8('EXCIT_RESU', 'COEF_MULT_C', iocc=iresu, scal=zc (jlccre+iresu-1),&
                        nbret=n)
252      continue
    endif
!
! --- EXCLUSION VECT_ASSE/CHARGE
!
    nbveas = lisnbg(lischa,'VECT_ASSE' )
    nbveag = lisnbg(lischa,'VECT_ASSE_GENE')
    nbveat = nbveas+nbveag
    call lisnnb(lischa, nbchar)
    if (nbveat .ne. 0) then
        if (nbveat .ne. nbchar) then
            call u2mess('F', 'CHARGES5_1')
        endif
    endif
!
end subroutine
