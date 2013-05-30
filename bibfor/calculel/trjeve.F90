subroutine trjeve(ific, nocc)
    implicit   none
    include 'asterc/getvis.h'
    include 'asterc/getvr8.h'
    include 'asterc/getvtx.h'
    include 'asterfort/assert.h'
    include 'asterfort/utest3.h'
    include 'asterfort/utesto.h'
    integer :: ific, nocc
! ----------------------------------------------------------------------
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
! ----------------------------------------------------------------------
!     COMMANDE:  TEST_RESU
!                MOT CLE FACTEUR "OBJET"
! ----------------------------------------------------------------------
    integer :: iocc, refi, refir, n1, n2, n2r, iret
    real(kind=8) :: epsi, epsir, refr, refrr
    character(len=3) :: ssigne
    character(len=8) :: crit
    character(len=16) :: tbtxt(2), tbref(2)
    character(len=24) :: nomobj
    integer :: iarg
    logical :: lref
!     ------------------------------------------------------------------
!
    do 100 iocc = 1, nocc
        call getvtx('OBJET', 'NOM', iocc, iarg, 1,&
                    nomobj, n1)
        call getvtx('OBJET', 'VALE_ABS', iocc, iarg, 1,&
                    ssigne, n1)
        call getvr8('OBJET', 'TOLE_MACHINE', iocc, iarg, 1,&
                    epsi, n1)
        call getvtx('OBJET', 'CRITERE', iocc, iarg, 1,&
                    crit, n1)
!
        call utest3('OBJET', iocc, tbtxt)
        lref=.false.
        call getvr8('OBJET', 'PRECISION', iocc, iarg, 1,&
                    epsir, iret)
        if (iret .ne. 0) then
            lref=.true.
            tbref(1)=tbtxt(1)
            tbref(2)=tbtxt(2)
            tbtxt(1)='NON_REGRESSION'
        endif
!
        write (ific,*) '---- OBJET '
        write (ific,*) '     ',nomobj
!
        call getvis('OBJET', 'VALE_CALC_I', iocc, iarg, 1,&
                    refi, n2)
        if (n2 .eq. 1) then
            call utesto(nomobj, 'I', tbtxt, refi, refr,&
                        epsi, crit, ific, .true., ssigne)
            if (lref) then
                call getvis('OBJET', 'VALE_REFE_I', iocc, iarg, 1,&
                            refir, n2r)
                call assert(n2.eq.n2r)
                call utesto(nomobj, 'I', tbref, refir, refrr,&
                            epsir, crit, ific, .false., ssigne)
            endif
        endif
!
        call getvr8('OBJET', 'VALE_CALC', iocc, iarg, 1,&
                    refr, n2)
        if (n2 .eq. 1) then
            call utesto(nomobj, 'R', tbtxt, refi, refr,&
                        epsi, crit, ific, .true., ssigne)
            if (lref) then
                call getvr8('OBJET', 'VALE_REFE', iocc, iarg, 1,&
                            refrr, n2r)
                call assert(n2.eq.n2r)
                call utesto(nomobj, 'R', tbref, refir, refrr,&
                            epsir, crit, ific, .false., ssigne)
            endif
        endif
        write (ific,*)' '
100  end do
!
!
end subroutine
