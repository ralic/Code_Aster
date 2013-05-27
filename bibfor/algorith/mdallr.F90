subroutine mdallr(resu1, resu2, basemo, nbmode, nbsauv,&
                  vecpr8, vecpc8, zcmplx)
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
!     ALLOCATION DES VECTEURS DE SORTIE (DONNEEES MODALES REELLES)
!     ------------------------------------------------------------------
! IN  : NOMRES : NOM DU CONCEPT RESULTAT
! IN  : NBMODE : NOMBRE DE MODES
! IN  : NBSAUV : NOMBRE DE PAS CALCULE (INITIAL COMPRIS)
! IN  : DATAx  : DONNEES MODALES AU COMPLET (x=I POUR ENTIER, x=K POUR
!                CHAR, x=R POUR REEL)
! ----------------------------------------------------------------------
    implicit none
!
    include 'jeveux.h'
!
    include 'asterc/getvtx.h'
    include 'asterfort/assert.h'
    include 'asterfort/crnslv.h'
    include 'asterfort/detrsd.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jeecra.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/juveca.h'
    include 'asterfort/nummo1.h'
    include 'asterfort/rsadpa.h'
    include 'asterfort/rsexch.h'
    include 'asterfort/rsnoch.h'
    include 'asterfort/vpcrea.h'
    include 'asterfort/vtcrem.h'
    include 'asterfort/wkvect.h'
    integer :: nbmode, nbsauv, ldlim, imode, ier, ibid, lvale, i, jrefa
    logical :: lrefe, zcmplx
    character(len=8) :: resu1, resu2, matgen, k8b, basemo, typ
    character(len=14) :: nugene
    character(len=19) :: chamge
    real(kind=8) :: vecpr8(nbmode, *)
    complex(kind=8) :: vecpc8(nbmode, *)
!
    integer :: nbmax, ipar, ipar1, ipar2
    parameter        (nbmax=50)
    character(len=24) :: kpar(nbmax)
    integer :: iarg
!
    call jemarq()
!
    lrefe = .true.
    nugene = resu2//'.NUGENE'
    matgen = '&&MDALMA'
!
! CREATION DE LA NUMEROTATION GENERALISE SUPPORT
    call nummo1(nugene, basemo, nbmode, 'PLEIN')
    call crnslv(nugene, 'LDLT', 'SANS', 'G')
!
! CREATION DE LA MATRICE GENERALISE SUPPORT
    call wkvect(matgen//'           .REFA', 'V V K24', 11, jrefa)
    zk24(jrefa-1+11)='MPI_COMPLET'
    zk24(jrefa-1+1)=basemo
    zk24(jrefa-1+2)=nugene
    zk24(jrefa-1+9) = 'MS'
    zk24(jrefa-1+10) = 'GENE'
    call wkvect(matgen//'           .LIME', 'V V K24', 1, ldlim)
    zk24(ldlim)=nugene
!
! recuperation des parametres a garder dans le modele gene
    call getvtx(' ', 'NOM_PARA', 1, iarg, nbmax,&
                kpar, ipar)
!
    do 100 imode = 1, nbsauv
!        --- VECTEUR PROPRE ---
        call rsexch(' ', resu2, 'DEPL', imode, chamge,&
                    ier)
        if (ier .eq. 0) then
        else if (ier .eq. 100 .and. lrefe) then
            if (.not. zcmplx) then
                call vtcrem(chamge, matgen, 'G', 'R')
            else
                call vtcrem(chamge, matgen, 'G', 'C')
            endif
! GLUTE CAR ON A UTILISE VTCRE[ABM] POUR UN CHAM_GENE QUI A UN .REFE
! DE TAILLE 2 ET NON 4 COMME UN CHAM_NO
            call juveca(chamge//'.REFE', 2)
        else
            call assert(.false.)
        endif
        call jeecra(chamge//'.DESC', 'DOCU', ibid, 'VGEN')
        call jeveuo(chamge//'.VALE', 'E', lvale)
        do 110 ier = 1, nbmode
            if (.not. zcmplx) then
                zr(lvale+ier-1) = vecpr8(ier,imode)
            else
                zc(lvale+ier-1) = vecpc8(ier,imode)
            endif
110      continue
        call rsnoch(resu2, 'DEPL', imode)
!
        do 200 i = 1, ipar
            call rsadpa(resu1, 'L', 1, kpar(i), imode,&
                        1, ipar1, typ)
            call rsadpa(resu2, 'E', 1, kpar(i), imode,&
                        0, ipar2, k8b)
            if (typ(1:1) .eq. 'I') then
                zi(ipar2) = zi(ipar1)
            else if (typ(1:1) .eq. 'R') then
                zr(ipar2) = zr(ipar1)
            else if (typ(1:2) .eq. 'K8') then
                zk8(ipar2) = zk8(ipar1)
            else if (typ(1:3) .eq. 'K16') then
                zk16(ipar2) = zk16(ipar1)
            else if (typ(1:3) .eq. 'K32') then
                zk32(ipar2) = zk32(ipar1)
            endif
200      continue
100  end do
!
    call vpcrea(0, resu2, ' ', ' ', ' ',&
                ' ', ier)
!
! --- MENAGE
    call detrsd('MATR_ASSE_GENE', matgen)
!
    call jedema()
!
end subroutine
