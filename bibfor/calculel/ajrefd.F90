subroutine ajrefd(resu1, resu2, action)
! ======================================================================
! COPYRIGHT (C) 1991 - 2010  EDF R&D                  WWW.CODE-ASTER.ORG
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
! person_in_charge: jacques.pellet at edf.fr
    implicit none
    include 'asterfort/assert.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jedupo.h'
    include 'asterfort/jeexin.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/wkvect.h'
    character(len=*) :: resu1, resu2, action
!----------------------------------------------------------------------
! BUT :
!  AJOUTER SI NECESSAIRE L'OBJET .REFD DANS LA SD_RESULTAT_DYN RESU2
!
!  SI ACTION=='FORCE' :
!      ON ALLOUE RESU2.REFD VIERGE S'IL N'EXISTE PAS DEJA
!
!  SI ACTION=='ZERO' OU 'COPIE', ON CREE RESU2.REFD SEULEMENT SI :
!     - RESU1 EST UNE SD_RESULTAT_DYN (I.E. IL POSSEDE .REFD)
!     - RESU2 NE POSSEDE PAS DEJA UN .REFD
!      SI ACTION=='ZERO' :
!         ON ALLOUE .REFD VIERGE
!      SI ACTION=='COPIE' :
!         ON COPIE RESU1.REFD DANS RESU2.REFD
!----------------------------------------------------------------------
!
    integer :: iexi1, iexi2, ibid
    character(len=19) :: r1, r2
!
    call jemarq()
!
    r1=resu1
    r2=resu2
!
    call jeexin(r2//'.REFD', iexi2)
    if (iexi2 .gt. 0) goto 9999
!
    if (action .eq. 'FORCE') then
        call wkvect(r2//'.REFD', 'G V K24', 7, ibid)
        goto 9999
    endif
!
    call jeexin(r1//'.REFD', iexi1)
    if (iexi1 .eq. 0) goto 9999
!
    if (action .eq. 'ZERO') then
        call wkvect(r2//'.REFD', 'G V K24', 7, ibid)
    else if (action.eq.'COPIE') then
        call jedupo(r1//'.REFD', 'G', r2//'.REFD', .false.)
    else
        call assert(.false.)
    endif
!
9999  continue
    call jedema()
!
end subroutine
