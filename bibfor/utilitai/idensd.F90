function idensd(typesd, sd1, sd2)
    implicit none
    logical :: idensd
    include 'jeveux.h'
    include 'asterfort/idenob.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/u2mesk.h'
    character(len=*) :: sd1, sd2, typesd
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: jacques.pellet at edf.fr
! ----------------------------------------------------------------------
!  BUT : DETERMINER L'IDENTITE DE 2 SD D'ASTER.
!  IN   TYPESD : TYPE DE  SD1 ET SD2
!               'PROF_CHNO'
!       SD1   : NOM DE LA 1ERE SD
!       SD2   : NOM DE LA 2EME SD
!
!     RESULTAT:
!       IDENSD : .TRUE.    SI SD1 == SD2
!                .FALSE.   SINON
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
    logical :: iden
    character(len=16) :: typ2sd
    character(len=19) :: pchn1, pchn2
!
! -DEB------------------------------------------------------------------
!
    call jemarq()
    typ2sd = typesd
    idensd=.true.
!
    if (sd1 .eq. sd2) goto 9999
!
!
    if (typ2sd .eq. 'PROF_CHNO') then
!     --------------------------------
        pchn1=sd1
        pchn2=sd2
        iden=idenob(pchn1//'.LILI',pchn2//'.LILI')
        if (.not.iden) goto 9998
        iden=idenob(pchn1//'.PRNO',pchn2//'.PRNO')
        if (.not.iden) goto 9998
        iden=idenob(pchn1//'.DEEQ',pchn2//'.DEEQ')
        if (.not.iden) goto 9998
        iden=idenob(pchn1//'.NUEQ',pchn2//'.NUEQ')
        if (.not.iden) goto 9998
!
!
    else
        call u2mesk('F', 'UTILITAI_47', 1, typ2sd)
    endif
!
    goto 9999
9998  continue
    idensd=.false.
!
!
9999  continue
    call jedema()
end function
