subroutine te0261(option, nomte)
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
! person_in_charge: samuel.geniaut at edf.fr
!.......................................................................
!
!     BUT: CALCUL DES CONTRAINTES AUX POINTS DE GAUSS
!          EN X-FEM
!
!          OPTION : 'SIEF_ELGA'
!
!     ENTREES  ---> OPTION : OPTION DE CALCUL
!              ---> NOMTE  : NOM DU TYPE ELEMENT
!.......................................................................
!
    implicit none
    include 'jeveux.h'
!
    include 'asterfort/assert.h'
    include 'asterfort/elref4.h'
    include 'asterfort/jevech.h'
    include 'asterfort/lteatt.h'
    include 'asterfort/teattr.h'
    include 'asterfort/xsidep.h'
    include 'asterfort/xteini.h'
    character(len=16) :: nomte, option
    character(len=8) :: enr, typmod(2)
    character(len=16) :: compor(4)
    integer :: ndim, nfh, nno, nnos, npg1, ipoids, ivf, idfde, jgano
    integer :: jpintt, jcnset, jheavt, jlonch, jbaslo, jlsn, jlst, jstno, jpmilt
    integer :: igeom, idepl, imate, jfisno, icont
    integer :: ddlc, nddl, nnom, nfe, ibid, ddls, ddlm, nfiss
!
!
! ---- CARACTERISTIQUES DU TYPE D'ELEMENT :
! ---- GEOMETRIE ET INTEGRATION
!      ------------------------
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
    call elref4(' ', 'RIGI', ndim, nno, nnos,&
                npg1, ipoids, ivf, idfde, jgano)
!
! ---- NOMBRE DE CONTRAINTES ASSOCIE A L'ELEMENT
!      -----------------------------------------
!     MATNS MAL DIMENSIONNEE
    call assert(nno.le.27)
!     INITIALISATION DES DIMENSIONS DES DDLS X-FEM
    call xteini(nomte, nfh, nfe, ibid, ddlc,&
                nnom, ddls, nddl, ddlm, nfiss,&
                ibid)
!
! - TYPE DE MODELISATION
    if (ndim .eq. 3) then
        typmod(1) = '3D      '
        typmod(2) = '        '
    else
        if (lteatt(' ','AXIS','OUI')) then
            typmod(1) = 'AXIS    '
        else if (lteatt(' ','C_PLAN','OUI')) then
            typmod(1) = 'C_PLAN  '
        else if (lteatt(' ','D_PLAN','OUI')) then
            typmod(1) = 'D_PLAN  '
        else
!          NOM D'ELEMENT ILLICITE
            call assert(lteatt(' ', 'C_PLAN', 'OUI'))
        endif
        if (nomte(1:2) .eq. 'MD') then
            typmod(2) = 'ELEMDISC'
        else if (nomte(1:2).eq.'MI') then
            typmod(2) = 'INCO    '
        else
            typmod(2) = '        '
        endif
    endif
!
!
! - PARAMETRES EN ENTREE
    call jevech('PGEOMER', 'L', igeom)
    call jevech('PMATERC', 'L', imate)
    call jevech('PDEPLAR', 'L', idepl)
    compor(1)=' '
    compor(2)=' '
    compor(3)=' '
    compor(4)=' '
!
!
!     PARAMETRES PROPRES A X-FEM
    call jevech('PPINTTO', 'L', jpintt)
    call jevech('PCNSETO', 'L', jcnset)
    call jevech('PHEAVTO', 'L', jheavt)
    call jevech('PLONCHA', 'L', jlonch)
    call jevech('PBASLOR', 'L', jbaslo)
    call jevech('PLSN', 'L', jlsn)
    call jevech('PLST', 'L', jlst)
    call jevech('PSTANO', 'L', jstno)
!     PROPRES AUX ELEMENTS 1D ET 2D (QUADRATIQUES)
    call teattr(nomte, 'S', 'XFEM', enr, ibid)
    if ((ibid.eq.0) .and. (nomte(3:4).ne.'AX') .and.&
        (enr.eq.'XH' .or.enr.eq.'XHT'.or.enr.eq.'XT'.or.enr.eq.'XHC') .and. ndim .le. 2) &
    call jevech('PPMILTO', 'L', jpmilt)
    if (nfiss .gt. 1) call jevech('PFISNO', 'L', jfisno)
!
    call jevech('PCONTRR', 'E', icont)
!
    call xsidep(nno, nfh, nfe, ddlc, ddlm,&
                igeom, typmod, zi(imate), compor, jpintt,&
                zi(jcnset), zi(jheavt), zi(jlonch), zr(jbaslo), idepl,&
                zr(jlsn), zr(jlst), zr(icont), jpmilt, nfiss,&
                jfisno)
end subroutine
