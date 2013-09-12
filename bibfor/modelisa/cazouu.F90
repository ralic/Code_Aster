subroutine cazouu(motfac, nzoco, nommcz)
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
    implicit none
#include "asterc/getmjm.h"
#include "asterfort/assert.h"
#include "asterfort/getvis.h"
#include "asterfort/getvr8.h"
#include "asterfort/getvtx.h"
#include "asterfort/u2mesk.h"
    character(len=16) :: motfac
    integer :: nzoco
    character(len=*) :: nommcz
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (TOUTES METHODES - LECTURE DONNEES)
!
! VERIFICATION DE L'UNICITE SUR TOUTES LES ZONES
! TRAITEMENT D'UN MOT-CLEF
!
! ----------------------------------------------------------------------
!
!
! IN  MOTFAC : MOT-CLE FACTEUR
! IN  NZOCO  : NOMBRE DE ZONES DE CONTACT
! IN  NOMMC  : NOM MOT-CLEF SIMPLE QUI DOIT ETRE IDENTIQUE
!
! ----------------------------------------------------------------------
!
    integer :: nmocl
    parameter   (nmocl=99)
!
    character(len=1) :: tt
    character(len=8) :: typmc
    character(len=16) :: nommc
    character(len=3) :: tymocl(nmocl)
    character(len=16) :: motcle(nmocl)
    logical :: error
    integer :: izone, noc, nval, ival, n
    real(kind=8) :: parar
    integer :: parai
    character(len=16) :: parak
    real(kind=8) :: parar1
    integer :: parai1
    character(len=16) :: parak1
    integer :: iarg
!
! ----------------------------------------------------------------------
!
    error = .false.
    izone = 1
    nommc = nommcz
    call getmjm(motfac, izone, 1, motcle, tymocl,&
                nval)
!
    nval = abs(nval)
    ASSERT(nval.lt.nmocl)
!
    if (nval .ne. 0) then
        do 15 izone = 1, nzoco
            call getmjm(motfac, izone, nval, motcle, tymocl,&
                        n)
            do 16 ival = 1, nval
                if (motcle(ival) .eq. nommc) then
                    typmc = tymocl(ival)
                    parai = 0
                    parar = 0.d0
                    parak = ' '
                    if (typmc(1:1) .eq. 'I') then
                        tt = 'I'
                        call getvis(motfac, nommc, iocc=izone, scal=parai, nbret=noc)
                    else if (typmc(1:2).eq.'TX') then
                        tt = 'T'
                        call getvtx(motfac, nommc, iocc=izone, scal=parak, nbret=noc)
                    else if (typmc(1:2).eq.'R8') then
                        tt = 'R'
                        call getvr8(motfac, nommc, iocc=izone, scal=parar, nbret=noc)
                    else
                        ASSERT(.false.)
                    endif
                    if (noc .eq. 0) then
                        goto 14
                    endif
!
                    if (izone .eq. 1) then
                        parai1 = parai
                        parar1 = parar
                        parak1 = parak
                    else
                        if (tt .eq. 'I') then
                            if (parai .ne. parai1) then
                                error = .true.
                                goto 20
                            endif
                        else if (tt.eq.'R') then
                            if (parar .ne. parar1) then
                                error = .true.
                                goto 20
                            endif
                        else if (tt.eq.'T') then
                            if (parak .ne. parak1) then
                                error = .true.
                                goto 20
                            endif
                        else
                            ASSERT(.false.)
                        endif
                    endif
14                  continue
                endif
16          continue
15      continue
    endif
!
20  continue
!
    if (error) then
        call u2mesk('F', 'CONTACT3_4', 1, nommc)
    endif
!
!
!
end subroutine
