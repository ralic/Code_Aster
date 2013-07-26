subroutine charme(fonree)
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
    implicit none
#include "jeveux.h"
#include "asterc/getfac.h"
#include "asterc/getres.h"
#include "asterfort/adalig.h"
#include "asterfort/alligr.h"
#include "asterfort/caarei.h"
#include "asterfort/cachre.h"
#include "asterfort/caddli.h"
#include "asterfort/caddlp.h"
#include "asterfort/cafaci.h"
#include "asterfort/cafono.h"
#include "asterfort/cagene.h"
#include "asterfort/cagrou.h"
#include "asterfort/caimch.h"
#include "asterfort/caliag.h"
#include "asterfort/caliai.h"
#include "asterfort/calich.h"
#include "asterfort/calicp.h"
#include "asterfort/caliel.h"
#include "asterfort/calimc.h"
#include "asterfort/caliob.h"
#include "asterfort/calirc.h"
#include "asterfort/caliso.h"
#include "asterfort/calyrc.h"
#include "asterfort/caprec.h"
#include "asterfort/carbe3.h"
#include "asterfort/caveas.h"
#include "asterfort/caveis.h"
#include "asterfort/cbchei.h"
#include "asterfort/cbelec.h"
#include "asterfort/cbimpd.h"
#include "asterfort/cblapl.h"
#include "asterfort/cbonde.h"
#include "asterfort/cbondp.h"
#include "asterfort/cbpesa.h"
#include "asterfort/cbprca.h"
#include "asterfort/cbpres.h"
#include "asterfort/cbrota.h"
#include "asterfort/cbsint.h"
#include "asterfort/cbvitn.h"
#include "asterfort/chveno.h"
#include "asterfort/cormgi.h"
#include "asterfort/initel.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeecra.h"
#include "asterfort/jeexin.h"
#include "asterfort/jeveuo.h"
#include "asterfort/u2mesk.h"
#include "asterfort/u2mess.h"
    character(len=4) :: fonree
!
! ----------------------------------------------------------------------
!
!      OPERATEURS :     AFFE_CHAR_MECA ET AFFE_CHAR_MECA_C
!                                      ET AFFE_CHAR_MECA_F
!
!      MOTS-CLES ACTUELLEMENT TRAITES:
!
!        MODELE
!        EPSA_CALCULEE
!        EVOL_CHAR
!        PESANTEUR
!        ROTATION
!        DDL_IMPO, FACE_IMPO, ARETE_IMPO,
!        LIAISON_DDL, LIAISON_OBLIQUE
!        FORCE_NODALE
!        CHARGE_REP: FORCE_CONTOUR FORCE_INTERNE FORCE_ARETE
!                    FORCE_FACE    FORCE_POUTRE  FORCE_COQUE
!        RELA_CINE_BP
!        PRE_INIT
!        PRES_REP
!        FLUX_THM_REP
!        FORCE_ELEC
!        INTE_ELEC
!        VITE_FACE
!        ONDE_FLUI
!        IMPE_FACE
!        ONDE_PLANE
!        CONTACT
!        LIAISON_GROUP
!        LIAISON_UNIF
!        LIAISON_SOLIDE
!        LIAISON_ELEM
!        LIAISON_CHAMNO
!        LIAISON_COQUE
!        LIAISON_MAIL
!        LIAISON_CYCL
!        LIAISON_INTERF
! ----------------------------------------------------------------------
    integer :: nbocc(6), i, iret, ndim, ibid, jlgrf
    character(len=5) :: param(7), para
    character(len=8) :: char, noma, nomo
    character(len=16) :: type, oper, chrep(6), motfac
    character(len=19) :: ligrch, ligrmo
!
    data chrep / 'FORCE_CONTOUR' , 'FORCE_INTERNE' , 'FORCE_ARETE' ,&
     &             'FORCE_FACE'    , 'FORCE_POUTRE'  , 'FORCE_COQUE'   /
    data param / 'F1D2D'         , 'F3D3D'         , 'F1D3D'       ,&
     &             'F2D3D'         , 'F1D1D'         , 'FCO3D'       ,&
     &             'FCO2D'         /
!     ------------------------------------------------------------------
!
!
    call getres(char, type, oper)
!
! - Mesh, Ligrel for model, dimension of model
!
    call cagene(char, oper, ligrmo, noma, ndim)
    nomo = ligrmo(1:8)
!
! - Ligrel for loads
!
    ligrch = char//'.CHME.LIGRE'
!
! - Keyword: FORCE_NODALE
!
    if (fonree .ne. 'COMP') then
        call alligr(char, oper, noma, fonree, ligrch)
        call cafono(char, ligrch, noma, ligrmo, fonree)
    endif
!
! --- CHARGES REPARTIES: FORCE_CONTOUR FORCE_INTERNE FORCE_ARETE
!                        FORCE_FACE    FORCE_POUTRE  FORCE_COQUE
!
    do i = 1, 6
        if (fonree .eq. 'COMP' .and. chrep(i) .ne. 'FORCE_POUTRE') then
            nbocc(i) = 0
        else
            call getfac(chrep(i), nbocc(i))
        endif
    end do
!
! --- VERIFICATION DE LA DIMENSION DES TYPE_ELEM DU MODELE ---
!
    if (ndim .gt. 3) call u2mess('A', 'MODELISA4_4')
!
    if (ndim .eq. 3) then
        do i = 1, 6
            if (nbocc(i) .ne. 0) then
!
                call cachre(char, ligrmo, noma, ndim, fonree,&
                            param(i), chrep(i))
!
            endif
        end do
!
    else
        do i = 4, 5
            if (nbocc(i) .ne. 0) then
!            --------- FORCE_FACE    INTERDIT EN 2D
!            --------- FORCE_POUTRE  INTERDIT EN 2D
                call u2mesk('A', 'MODELISA4_5', 1, chrep(i))
            endif
        end do
        do i = 1, 6
            if (nbocc(i) .ne. 0) then
!
                para = param(i)
!    CAS DE FORCE INTERNE EN 2D
                if (i .eq. 2) para = 'F2D2D'
!    CAS DES COQCYL AXI
                if (i .eq. 6 .and. ndim .eq. 2) para = 'FCO2D'
                call cachre(char, ligrmo, noma, ndim, fonree,&
                            para, chrep(i))
            endif
        end do
    endif
!
    if (fonree .ne. 'COMP') then
!         ================
!
! --- DEFORMATION INITIALE ----
!
        call cbchei(char, noma, ligrmo, fonree)
!
! --- PRE_SIGM----
!
        call cbsint(char, noma, ligrmo, fonree)
!
! --- PRESSION ---
!
        call cbpres(char, noma, ligrmo, ndim, fonree)
!
! --- VITE_FACE ---
!
        call cbvitn(char, noma, ligrmo, fonree)
!
! --- IMPE_FACE ---
!
        call cbimpd(char, noma, ligrmo, fonree)
!
    endif
!
! --- TEMPERATURE, PRESSION, PESANTEUR, ROTATION, FORCES ELECTROS
!     DEFORMATIONS PLANES GENERALISEES, LIAISON UNILATERALE,
!     DEFORMATIONS ANELASTIQUES, RELA_CINE_BP ---
!
    if (fonree .eq. 'REEL') then
!         ================
        call cbprca(char)
        call cbpesa(char, noma, ndim, ligrmo)
        call cbrota(char, noma, ndim, ligrmo)
        call caprec(char, noma)
!
! --- FORCE_ELEC ----
!
        call cbelec(char, ligrmo, noma)
!
! --- FORCES DE LAPLACE ----
!
        call cblapl(char, ligrmo, noma)
!
! --- ONDE_FLUI ---
!
        call cbonde(char, noma, ligrmo, fonree)
!
! --- DDL_POUTRE ---
!
        call caddlp(fonree, char)
!
    endif
!
! --- ONDE_PLANE ---
!
    if (fonree .eq. 'FONC') then
!         ================
        call cbondp(char, noma)
    endif
!
! --- DDL_IMPO ---
!
    motfac = 'DDL_IMPO'
    call caddli(motfac, char, noma, ligrmo, fonree)
!
! --- LIAISON_DDL ---
!
    call caliai(fonree, char)
!
    if (fonree .eq. 'REEL') then
!         ================
!
! --- LIAISON_MAIL ---
!
        call calirc(char)
!
! --- LIAISON_CYCL ---
!
        call calyrc(char)
!
! --- LIAISON_ELEM ---
!
        call caliel(fonree, char)
!
! --- LIAISON_CHAMNO ---
!
        call calich(char)
!
! --- LIAISON_RBE3 ---
!
        call carbe3(char)
!
! --- VECT_ASSE ---
!
        call caveas(char)
!
! --- VECT_ISS ---
!
        call caveis(char)
!
! --- CHAMNO_IMPO ---
!
        call caimch(char)
!
! --- LIAISON_INTERF ---
!
        call calimc(char)
!
! --- ARETE_IMPO ---
!
        call caarei(char, noma, ligrmo, fonree)
!
    endif
!
    if (fonree .ne. 'COMP') then
!         ================
!
! --- FACE_IMPO ---
        call cafaci(char, noma, ligrmo, fonree)

!
! --- LIAISON_OBLIQUE ---
        call caliob(char, noma, ligrmo, fonree)
!
! --- LIAISON_GROUP ---
        call caliag(fonree, char)
!
! --- LIAISON_UNIF ---
        call cagrou(fonree, char)
!
! --- LIAISON_SOLIDE ---
        call caliso(char)
!
! --- LIAISON_COQUE ---
        call calicp(char)
!
    endif
!
!
! --- MISE A JOUR DU LIGREL DE CHARGE :
    call jeexin(ligrch//'.LGRF', iret)
    if (iret .ne. 0) then
        call adalig(ligrch)
        call cormgi('G', ligrch)
        call jeecra(ligrch//'.LGRF', 'DOCU', ibid, 'MECA')
        call initel(ligrch)
!       -- LIEN ENTRE LE LIGREL DE CHARGE ET LE MODELE :
        call jeveuo(ligrch//'.LGRF', 'E', jlgrf)
        zk8(jlgrf-1+2)=ligrmo(1:8)
    endif
!
!
    if (fonree .ne. 'COMP') then
!       -- VERIFICATION DES NORMALES AUX MAILLES SURFACIQUES EN 3D
!       -- ET LINEIQUES EN 2D
        call chveno(fonree, noma, nomo)
    endif
!
end subroutine
