subroutine te0414(optioz, nomtz)
    implicit none
    include 'jeveux.h'
    include 'asterfort/cosiro.h'
    include 'asterfort/jevech.h'
    include 'asterfort/jevete.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/matpgl.h'
    include 'asterfort/tranlg.h'
    include 'asterfort/u2mesk.h'
    include 'asterfort/u2mess.h'
    include 'asterfort/vdgnlr.h'
    include 'asterfort/vdpnlr.h'
    include 'asterfort/vdxnlr.h'
    character(len=*) :: optioz, nomtz
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!     ----------------------------------------------------------------
!     CALCUL DES OPTIONS DES ELEMENTS DE COQUE : COQUE_3D
!     ----------------------------------------------------------------
!
    integer :: nb1, jcret, codret
    real(kind=8) :: matloc(51, 51), plg(9, 3, 3)
    logical :: matric
    character(len=16) :: option, nomte
! ----------------------------------------------------------------------
!
!-----------------------------------------------------------------------
    integer :: i, i1, i2, ibid, icompo, ideplm, ideplp
    integer :: jgeom, jmatr, lzi, lzr, nb2, nddlet
!-----------------------------------------------------------------------
    option = optioz
    nomte = nomtz
!
    call jeveuo('&INEL.'//nomte(1:8)//'.DESI', 'L', lzi)
    nb2 = zi(lzi-1+2)
!
    if (option .eq. 'RAPH_MECA' .or. option(1:9) .eq. 'FULL_MECA' .or. option(1:9) .eq.&
        'RIGI_MECA') then
!        -- PASSAGE DES CONTRAINTES DANS LE REPERE INTRINSEQUE :
        call cosiro(nomte, 'PCONTMR', 'L', 'UI', 'G',&
                    ibid, 'S')
    endif
!
!
    matric = ( option(1:9) .eq.'FULL_MECA' .or. option(1:10).eq.'RIGI_MECA_' )
!
    call jevech('PGEOMER', 'L', jgeom)
    call jevech('PDEPLMR', 'L', ideplm)
    call jevech('PDEPLPR', 'L', ideplp)
    call jevech('PCOMPOR', 'L', icompo)
!
    if (zk16(icompo+3)(1:9) .eq. 'COMP_ELAS') then
!          ------------------------------------
!       SEULE RELATION ADMISE : ELAS
        if (zk16(icompo)(1:5) .ne. 'ELAS ') then
            call u2mesk('F', 'ELEMENTS5_46', 1, zk16(icompo))
        endif
!
!
! ------ HYPER-ELASTICITE
!
        if (zk16 ( icompo + 2 ) .eq. 'GROT_GDEP') then
!
! --------- DEFORMATION DE GREEN
!
            call vdgnlr(option, nomte)
!
            goto 9999
!
        else
!
! --------- AUTRES MESURES DE DEFORMATIONS
!
            call u2mesk('F', 'ELEMENTS3_93', 1, zk16(icompo+2))
!
        endif
!
    else if (zk16(icompo+3)(1:9) .eq. 'COMP_INCR') then
!              ------------------------------------
!
        if (zk16(icompo+2) .eq. 'GROT_GDEP') then
!
! --------- HYPO-ELASTICITE
!
            call vdpnlr(option, nomte, codret)
!
            goto 9999
!
        else if (zk16(icompo+2)(6:10) .eq. '_REAC') then
!
            call u2mess('A', 'ELEMENTS3_94')
!
            do 90 i = 1, nb2-1
                i1=3*(i-1)
                i2=6*(i-1)
                zr(jgeom+i1) = zr(jgeom+i1) +zr(ideplm+i2) +zr(ideplp+ i2)
                zr(jgeom+i1+1) = zr(jgeom+i1+1)+zr(ideplm+i2+1) +zr(ideplp+i2+1)
                zr(jgeom+i1+2) = zr(jgeom+i1+2)+zr(ideplm+i2+2) +zr(ideplp+i2+2)
90          continue
        endif
!
        call vdxnlr(option, nomte, zr(jgeom), matloc, nb1,&
                    codret)
!
        if (matric) then
!
            call jevech('PMATUUR', 'E', jmatr)
!
! --------- MATRICE DE PASSAGE REPERE GLOBAL REPERE LOCAL
!
            call jevete('&INEL.'//nomte(1:8)//'.DESR', ' ', lzr)
            call matpgl(nb2, zr(lzr), plg)
!
! --------- OPERATION DE TRANFORMATION DE MATLOC DANS LE REPERE GLOBAL
!           ET STOCKAGE DANS ZR
!
            nddlet = 6*nb1+3
            call tranlg(nb1, 51, nddlet, plg, matloc,&
                        zr(jmatr))
        endif
!
    endif
!
    if (option(1:9) .eq. 'RAPH_MECA' .or. option(1:9) .eq. 'FULL_MECA') then
        call jevech('PCODRET', 'E', jcret)
        zi(jcret) = codret
    endif
!
9999  continue
!
    if (option .eq. 'RAPH_MECA' .or. option(1:9) .eq. 'FULL_MECA') then
!        -- PASSAGE DES CONTRAINTES DANS LE REPERE UTILISATEUR :
        call cosiro(nomte, 'PCONTPR', 'E', 'IU', 'G',&
                    ibid, 'R')
    endif
!
end subroutine
