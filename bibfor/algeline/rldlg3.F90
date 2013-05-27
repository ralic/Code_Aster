subroutine rldlg3(metres, lmat, xsol, cxsol, nbsol)
    implicit none
    include 'jeveux.h'
!
    include 'asterfort/assert.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jelira.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/mtdsc2.h'
    include 'asterfort/rldlc8.h'
    include 'asterfort/rldlr8.h'
    include 'asterfort/rlduc8.h'
    include 'asterfort/rldur8.h'
    include 'asterfort/rlfc16.h'
    include 'asterfort/rltfr8.h'
    character(len=*) :: metres
    integer :: lmat, nbsol
    real(kind=8) :: xsol(*)
    complex(kind=8) :: cxsol(*)
!     ------------------------------------------------------------------
!            CONFIGURATION MANAGEMENT OF EDF VERSION
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
! ======================================================================
!
    integer :: typvar, typsym
    character(len=19) :: mat19
    character(len=8) :: kbid
!------------------------------------------------------------------
!-----------------------------------------------------------------------
    integer :: jscbl, jscdi, jschc, nbbloc, neq
!-----------------------------------------------------------------------
    call jemarq()
    neq = zi(lmat+2)
    typvar = zi(lmat+3)
    typsym = zi(lmat+4)
    mat19=zk24(zi(lmat+1))
!
    if (metres .eq. 'LDLT') then
!     ---------------------------------
        call jelira(mat19//'.UALF', 'NMAXOC', nbbloc, kbid)
        call mtdsc2(mat19, 'SCHC', 'L', jschc)
        call mtdsc2(mat19, 'SCDI', 'L', jscdi)
        call mtdsc2(mat19, 'SCBL', 'L', jscbl)
!
        if (typvar .eq. 1) then
!           --- SYSTEME REELLE ---
!
!         -- CAS D'UNE MATRICE SYMETRIQUE
            if (typsym .eq. 1) then
                call rldlr8(mat19, zi(jschc), zi(jscdi), zi(jscbl), neq,&
                            nbbloc, xsol, nbsol)
!
!        -- CAS D'UNE MATRICE NON_SYMETRIQUE
            else if (typsym.eq.0) then
                call rldur8(mat19, zi(jschc), zi(jscdi), zi(jscbl), neq,&
                            nbbloc/2, xsol, nbsol)
            endif
!
        else if (typvar.eq.2) then
!         -- SYSTEME COMPLEXE (SYMETRIQUE) ---
            if (typsym .eq. 1) then
                call rldlc8(mat19, zi(jschc), zi(jscdi), zi(jscbl), neq,&
                            nbbloc, cxsol, nbsol)
!        -- CAS D'UNE MATRICE NON_SYMETRIQUE
            else if (typsym.eq.0) then
                call rlduc8(mat19, zi(jschc), zi(jscdi), zi(jscbl), neq,&
                            nbbloc/2, cxsol, nbsol)
            endif
        endif
!
!
    else if (metres.eq.'MULT_FRONT') then
!     ------------------------------------
        if (typvar .eq. 1) then
            call rltfr8(mat19, neq, xsol, nbsol, typsym)
        else if (typvar.eq.2) then
            call rlfc16(mat19, neq, cxsol, nbsol, typsym)
        endif
    else
        call assert(.false.)
    endif
!
    call jedema()
!
end subroutine
