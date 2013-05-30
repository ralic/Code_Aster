subroutine xvelfm(nfiss, fiss, modx)
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! ======================================================================
!
    implicit none
    include 'jeveux.h'
    include 'asterfort/exixfe.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/u2mesk.h'
    include 'asterfort/xvfimo.h'
    integer :: nfiss
    character(len=8) :: fiss(nfiss), modx
!
! ----------------------------------------------------------------------
!
! ROUTINE XFEM (VERIFICATION DES SD)
!
! VERIFICATION QUE LES FISSURES OBTENUES EN SCRUTANT UN MOT-CLE ET
! STOCKEES DANS LA LISTE FISS(1:NFISS), APPARTIENNENT BIEN AU MODELE
! MODX
!
!   -> ON S'ASSURE AU PREALABLE QUE MODX EST UN MODELE X-FEM
!
! ----------------------------------------------------------------------
!
! IN  NFISS  : NOMBRE DE FISSURES
! IN  FISS   : LISTE DES NOMS DES FISSURES
! IN  MODX   : NOM DU MODELE EN ENTREE
!
! ----------------------------------------------------------------------
!
    integer :: ifiss, iret
    logical :: ltrouv
    character(len=8) :: valk(2)
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
!     SI LE MODELE N'EST PAS UN MODELE X-FEM -> ERREUR FATALE
    call exixfe(modx, iret)
    if (iret .eq. 0) then
        valk(1)=modx
        call u2mesk('F', 'XFEM_72', 1, valk)
    endif
!
!     BOUCLE SUR LES FISSURES IN
    do 100 ifiss = 1, nfiss
!
        ltrouv=xvfimo(modx,fiss(ifiss))
!
!       SI FISS(IFISS) EST ABSENTE DU MODELE -> ERREUR FATALE
        if (.not.ltrouv) then
            valk(1)=fiss(ifiss)
            valk(2)=modx
            call u2mesk('F', 'XFEM_73', 2, valk)
        endif
!
100  end do
!
    call jedema()
end subroutine
