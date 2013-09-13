subroutine usuban(mater, isupp, para, ier)
    implicit none
#include "asterfort/getvtx.h"
#include "asterfort/lxlgut.h"
#include "asterfort/u2mesg.h"
    real(kind=8) :: para(*)
    character(len=*) :: mater
!-----------------------------------------------------------------------
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
!-----------------------------------------------------------------------
!     BANQUE DE DONNEES
! IN  : MATE   : DEFINITION DU COUPLE DE MATERIAUX
! IN  : ISUPP  : RECUPERATION DES COEFFICIENTS TUBE ( ISUPP = 1 )
!                                  OU DE L'OBSTACLE ( ISUPP = 2 )
! OUT : PARA   : DEFINITION DES COEFFICIENTS
! OUT : IER    : CODE RETOUR
!-----------------------------------------------------------------------
    character(len=24) :: loi, mate, type
    character(len=24) :: valk(3)
!
!-----------------------------------------------------------------------
    integer :: ier, isupp, lk, lt, n1, n2
!
!-----------------------------------------------------------------------
    ier = 0
    mate = mater
    call getvtx(' ', 'LOI_USURE', scal=loi, nbret=n1)
    call getvtx(' ', 'CONTACT', scal=type, nbret=n2)
!
! **********************************************************************
!                 M O D E L E     A R C H A R D
! **********************************************************************
!
    if (loi(1:7) .eq. 'ARCHARD') then
        para(1) = 9999.d0
!
        if (type .eq. 'TUBE_BAV') then
            if (mate .eq. 'I600_I600') then
                if (isupp .eq. 1) then
                    para(1) = 1.2d-13
                else
                endif
            else if (mate .eq. 'I600TT_I600') then
                if (isupp .eq. 1) then
                    para(1) = 4.5d-14
                else
                endif
            else if (mate .eq. 'I600TT_I600TT') then
                if (isupp .eq. 1) then
                    para(1) = 1.4d-15
                else
                endif
            else if (mate .eq. 'I600_I600CR') then
                if (isupp .eq. 1) then
                    para(1) = 7.2d-14
                else
                endif
            else if (mate .eq. 'I600TT_I600CR') then
                if (isupp .eq. 1) then
                    para(1) = 9.1d-16
                else
                endif
            else if (mate .eq. 'I690TT_I600CR') then
                if (isupp .eq. 1) then
                    para(1) = 1.2d-15
                else
                endif
            else if (mate .eq. 'I600_Z10C13') then
                if (isupp .eq. 1) then
                    para(1) = 9.9d-14
                else
                endif
            else if (mate .eq. 'I600_A405') then
                if (isupp .eq. 1) then
                    para(1) = 6.2d-14
                else
                endif
            else if (mate .eq. 'I690_A405') then
                if (isupp .eq. 1) then
                    para(1) = 4.1d-16
                else
                endif
            else if (mate .eq. 'I600TT_Z6C13') then
                if (isupp .eq. 1) then
                    para(1) = 9.2d-15
                else
                endif
            else if (mate .eq. 'I600_Z6C13') then
                if (isupp .eq. 1) then
                    para(1) = 7.1d-15
                else
                endif
            else if (mate .eq. 'I690_Z6C13') then
                if (isupp .eq. 1) then
                    para(1) = 7.7d-15
                else
                endif
            else if (mate .eq. 'I600_A347') then
                if (isupp .eq. 1) then
                    para(1) = 1.0d-13
                else
                endif
            endif
!
        else if (type .eq. 'TUBE_ALESAGE') then
            if (mate .eq. 'I690_Z10C13') then
                if (isupp .eq. 1) then
                    para(1) = 6.0d-17
                else
                endif
            else if (mate .eq. 'I600_I600') then
                if (isupp .eq. 1) then
                    para(1) = 1.6d-13
                else
                endif
            else if (mate .eq. 'I690_I600') then
                if (isupp .eq. 1) then
                    para(1) = 5.2d-14
                else
                endif
            else if (mate .eq. 'I600_I600CR') then
                if (isupp .eq. 1) then
                    para(1) = 2.2d-15
                else
                endif
            else if (mate .eq. 'I690_I600CR') then
                if (isupp .eq. 1) then
                    para(1) = 4.4d-15
                else
                endif
            endif
!
        else if (type .eq. 'TUBE_4_ENCO') then
            if (mate .eq. 'I600_Z10C13') then
                if (isupp .eq. 1) then
                    para(1) = 2.4d-16
                else
                endif
            else if (mate .eq. 'I690_Z10C13') then
                if (isupp .eq. 1) then
                    para(1) = 8.2d-17
                else
                endif
            else if (mate .eq. 'I600_A405') then
                if (isupp .eq. 1) then
                    para(1) = 6.5d-14
                else
                endif
            else if (mate .eq. 'I600TT_A405') then
                if (isupp .eq. 1) then
                    para(1) = 1.4d-15
                else
                endif
            else if (mate .eq. 'I690_A405') then
                if (isupp .eq. 1) then
                    para(1) = 7.8d-15
                else
                endif
            endif
!
        else if (type .eq. 'TUBE_3_ENCO') then
            if (mate .eq. 'I600_Z10C13') then
                if (isupp .eq. 1) then
                    para(1) = 2.5d-16
                else
                endif
            else if (mate .eq. 'I690_Z10C13') then
                if (isupp .eq. 1) then
                    para(1) = 2.4d-16
                else
                endif
            endif
!
        else if (type .eq. 'TUBE_TUBE') then
            if (mate .eq. 'I600_I600') then
                if (isupp .eq. 1) then
                    para(1) = 1.8d-13
                else
                endif
            else if (mate .eq. 'I690_I690') then
                if (isupp .eq. 1) then
                    para(1) = 1.0d-12
                else
                endif
            endif
!
        else if (type .eq. 'GRAPPE_ALESAGE') then
            if (mate .eq. 'A304L_A304L') then
                if (isupp .eq. 1) then
                    para(1) = 10.d-15
                else
                    para(1) = 15.d-15
                endif
            else if (mate .eq. 'A316L_A304L') then
                if (isupp .eq. 1) then
                    para(1) = 10.d-15
                else
                    para(1) = 5.d-15
                endif
            else if (mate .eq. 'NITRURE_A304L') then
                if (isupp .eq. 1) then
                    para(1) = 0.1d-15
                else
                    para(1) = 70.d-15
                endif
            else if (mate .eq. 'CHROME_A304L') then
                if (isupp .eq. 1) then
                    para(1) = 0.1d-15
                else
                    para(1) = 50.d-15
                endif
            endif
!
        else if (type .eq. 'GRAPPE_1_ENCO') then
            if (mate .eq. 'A304L_A304L') then
                if (isupp .eq. 1) then
                    para(1) = 20.d-15
                else
                    para(1) = 20.d-15
                endif
            else if (mate .eq. 'A316L_A304L') then
                if (isupp .eq. 1) then
                    para(1) = 30.d-15
                else
                    para(1) = 20.d-15
                endif
            endif
        endif
!
        if (para(1) .eq. 9999d0) then
            ier = ier + 1
            lk = lxlgut(mate)
            lt = lxlgut(type)
            valk (1) = type(1:lt)
            valk (2) = mate(1:lk)
            valk (3) = ' '
            call u2mesg('E', 'PREPOST5_77', 3, valk, 0,&
                        0, 0, 0.d0)
        endif
!
! **********************************************************************
!                 M O D E L E     K W U _ E P R I
! **********************************************************************
!
    else if (loi(1:8) .eq. 'KWU_EPRI') then
!        --- 'COEF_USURE_...' ---
        para(1) = 9999.d0
!        --- 'COEF_GLISSEMENT_...' ---
        para(2) = 9999.d0
!        --- 'COEF_IMPACT_...' ---
        para(3) = 9999.d0
!        --- 'CST_K_...' ---
        para(4) = 9999.d0
!        --- 'CST_C_...' ---
        para(5) = 9999.d0
        if (type .eq. 'TUBE_BAV') then
            if (mate .eq. 'I600_I600') then
                if (isupp .eq. 1) then
                    para(1) = 1.4d-15
                    para(2) = 1.4d-15
                    para(3) = 1.4d-15
                    para(4) = 5.d0
                    para(5) = 10.d0
                else
                endif
            endif
        else if (type .eq. 'GRAPPE_ALESAGE') then
        endif
        if (para(1) .eq. 9999d0) then
            ier = ier + 1
            lk = lxlgut(mate)
            lt = lxlgut(type)
            valk (1) = type(1:lt)
            valk (2) = mate(1:lk)
            valk (3) = ' '
            call u2mesg('E', 'PREPOST5_77', 3, valk, 0,&
                        0, 0, 0.d0)
        endif
!
! **********************************************************************
!                 M O D E L E     E D F _ M Z
! **********************************************************************
!
    else if (loi(1:6) .eq. 'EDF_MZ') then
!        --- 'COEF_USURE_...' ---
        para(1) = 9999.d0
!        --- 'SEUIL_...' ---
        para(2) = 9999.d0
!        --- 'EXPOSANT_USURE_...' ---
        para(3) = 9999.d0
!        --- 'TAUX_RALENTISSEMENT_...' ---
        para(4) = 9999.d0
        if (type .eq. 'TUBE_BAV') then
            if (mate .eq. 'I600_I600') then
                if (isupp .eq. 1) then
                    para(1) = 1.d-13
                    para(2) = 1.14d-16
                    para(3) = 1.2d0
                    para(4) = 2.44d-08
                else
                endif
            endif
        else if (type .eq. 'GRAPPE_ALESAGE') then
        endif
        if (para(1) .eq. 9999d0) then
            ier = ier + 1
            lk = lxlgut(mate)
            lt = lxlgut(type)
            valk (1) = type(1:lt)
            valk (2) = mate(1:lk)
            valk (3) = ' '
            call u2mesg('E', 'PREPOST5_77', 3, valk, 0,&
                        0, 0, 0.d0)
        endif
!
    endif
!
end subroutine
