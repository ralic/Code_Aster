subroutine nmflin(sdpost, matass, freqr, linsta)
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
    include 'jeveux.h'
    include 'asterfort/assert.h'
    include 'asterfort/echmat.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/nmlesd.h'
    include 'asterfort/u2mess.h'
    character(len=19) :: sdpost
    character(len=19) :: matass
    logical :: linsta
    real(kind=8) :: freqr
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (POST-TRAITEMENT)
!
! DETECTION D'UNE INSTABILITE
!
! ----------------------------------------------------------------------
!
!
! IN  MATASS : MATRICE ASSEMBLEE
! IN  SDPOST : SD POUR POST-TRAITEMENTS (CRIT_STAB ET MODE_VIBR)
! IN  FREQR  : FREQUENCE SOLUTION INSTABILITE
! OUT LINSTA : .TRUE. SI INSTABILITE DETECTEE
!
!
!
!
    logical :: valtst, ldist
    character(len=24) :: k24bid
    real(kind=8) :: freqr0, prec, r8bid, minmat, maxmat
    character(len=16) :: optrig, sign
    integer :: jrefa, ibid
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATIONS
!
    linsta = .false.
!
! --- PARAMETRES
!
    call nmlesd('POST_TRAITEMENT', sdpost, 'SOLU_FREQ_FLAM', ibid, freqr0,&
                k24bid)
    if (abs(freqr0) .gt. 1.d30) freqr0=freqr
    call nmlesd('POST_TRAITEMENT', sdpost, 'RIGI_GEOM_FLAMB', ibid, r8bid,&
                optrig)
    call nmlesd('POST_TRAITEMENT', sdpost, 'PREC_INSTAB', ibid, prec,&
                k24bid)
    call nmlesd('POST_TRAITEMENT', sdpost, 'SIGN_INSTAB', ibid, r8bid,&
                sign)
!
! --- DETECTION INSTABILITE
!
    if (optrig .eq. 'RIGI_GEOM_NON') then
        call jeveuo(matass//'.REFA', 'L', jrefa)
        if (zk24(jrefa-1+11)(1:11) .ne. 'MPI_COMPLET') call u2mess('F', 'MECANONLINE6_13')
        ldist = .false.
        call echmat(matass, ldist, minmat, maxmat)
        if (((freqr0*freqr).lt.0.d0) .or. (abs(freqr).lt.(prec*minmat))) then
            linsta = .true.
        endif
    else
        valtst = .false.
        if (sign .eq. 'POSITIF') then
            valtst = ((freqr.ge.0.d0).and.(abs(freqr).lt.(1.d0+prec)))
        else if (sign.eq.'NEGATIF') then
            valtst = ((freqr.le.0.d0).and.(abs(freqr).lt.(1.d0+prec)))
        else if (sign.eq.'POSITIF_NEGATIF') then
            valtst = (abs(freqr).lt.(1.d0+prec))
        else
            call assert(.false.)
        endif
        if (valtst) linsta = .true.
    endif
!
    call jedema()
end subroutine
