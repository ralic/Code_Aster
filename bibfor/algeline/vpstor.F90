subroutine vpstor(ineg, type, modes, nbmode, neq,&
                  vecpr8, vecpc8, mxresf, nbpari, nbparr,&
                  nbpark, nopara, mod45, resufi, resufr,&
                  resufk, iprec)
    implicit none
#include "jeveux.h"
#include "asterc/getres.h"
#include "asterfort/dismoi.h"
#include "asterfort/exisd.h"
#include "asterfort/getvid.h"
#include "asterfort/indk24.h"
#include "asterfort/jedema.h"
#include "asterfort/jeecra.h"
#include "asterfort/jeexin.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/juveca.h"
#include "asterfort/refdaj.h"
#include "asterfort/rsadpa.h"
#include "asterfort/rsexch.h"
#include "asterfort/rsexis.h"
#include "asterfort/rsnoch.h"
#include "asterfort/utmess.h"
#include "asterfort/vtcreb.h"
#include "asterfort/vtcrem.h"
#include "blas/dcopy.h"
#include "blas/zcopy.h"
!
    integer :: ineg, nbmode, neq, mxresf, nbpari, nbparr, nbpark
    integer :: iprec, resufi(mxresf, *)
    character(len=4) :: mod45
    character(len=*) :: type, modes, resufk(mxresf, *), nopara(*)
    real(kind=8) :: vecpr8(neq, *), resufr(mxresf, *)
    complex(kind=8) :: vecpc8(neq, *)
!     ------------------------------------------------------------------
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
!     STOCKAGE DES VALEURS PROPRES
!
!     REMARQUE:
!        DANS NOPARA, ON A LES NOMS DE PARAMETRES DE TYPE ENTIER
!                     ENSUITE LES NOMS DE PARAMETRES DE TYPE CHARACTER
!                     ENSUITE LES NOMS DE PARAMETRES DE TYPE REEL
!     ------------------------------------------------------------------
!     ------------------------------------------------------------------
    integer :: imode, jmode, ier, nmin, imin, nmax, imax
    integer :: vali(3), jpara
    integer :: nmin1, kmode, nordr, iarg, i, ladpa, lmode, lvale
    integer :: nbpast, irang, iret, jmodg, jmacr, jbasm
    integer :: jmod2, jlime
    parameter    ( nbpast = 19 )
    character(len=8) :: res, k8b, modele, chmat, carael, basemo
    character(len=16) :: typcon, nomcmd, nosy, typmod
    character(len=19) :: chamno, sd2
    character(len=24) :: nume, nopast(nbpast)
    character(len=24) :: valk, typeba, raide, raide2, k24b
    logical :: lrefd, lbasm, lstock
    character(len=24), pointer :: rerr(:) => null()
!     ------------------------------------------------------------------
! --- PARAMETRES STOCKES DANS LA SD RESULTAT DYNAMIQUE
    data  nopast /        'NUME_MODE'       ,&
     &  'NORME'           , 'TYPE_MODE'       , 'NOEUD_CMP'       ,&
     &  'FREQ'            , 'OMEGA2'          , 'AMOR_REDUIT'     ,&
     &  'MASS_GENE'       , 'RIGI_GENE'       , 'AMOR_GENE'       ,&
     &  'MASS_EFFE_DX'    , 'MASS_EFFE_DY'    , 'MASS_EFFE_DZ'    ,&
     &  'FACT_PARTICI_DX' , 'FACT_PARTICI_DY' , 'FACT_PARTICI_DZ' ,&
     &  'MASS_EFFE_UN_DX' , 'MASS_EFFE_UN_DY' , 'MASS_EFFE_UN_DZ' /
!     ------------------------------------------------------------------
!
    call jemarq()
!
    call getres(res, typcon, nomcmd)
!
!     POUR POUVOIR UTILISER VPSTOR DANS STAT_NON_LINE VIA NMOP45
    if (typcon .eq. 'EVOL_NOLI') then
        typcon = 'MODE_FLAMB'
        if (mod45 .eq. 'VIBR') typcon = 'MODE_MECA'
        if (mod45 .eq. 'STAB') typcon = 'MODE_STAB'
    endif
!
    if (typcon .eq. 'MODE_ACOU') then
        nosy = 'PRES'
    else
        nosy = 'DEPL'
    endif
!
    lbasm = .false.
    lrefd = .true.
    lstock = .false.
!
    call jeexin(modes(1:8)//'           .REFD', iret)
    if (iret .eq. 0) then
        call refdaj(' ', modes, -1, ' ', 'INIT',&
                    ' ', iret)
        lrefd = .false.
    endif
!
    typeba = ' '
    call dismoi('TYPE_BASE', modes, 'RESU_DYNA', repk=typeba, arret='C',&
                ier=iret)
    if (typeba(1:1) .ne. ' ') lbasm = .true.
    if (lbasm) then
        call getvid(' ', 'RAIDE', scal=raide, nbret=ier)
        if (ier .ne. 0) then
            call dismoi('NOM_NUME_DDL', raide, 'MATR_ASSE', repk=nume)
        else 
            call dismoi('REF_RIGI_PREM', modes, 'RESU_DYNA', repk=raide, arret='C')
            call dismoi('NUME_DDL', modes, 'RESU_DYNA', repk=nume, arret='C')
        endif
    else
        call dismoi('REF_RIGI_PREM', modes, 'RESU_DYNA', repk=k24b, arret='C',&
                    ier=iret)
        raide = k24b(1:8)
        call exisd('MATR_ASSE', raide, iret)
        if (iret .ne. 0) then
            call dismoi('NOM_NUME_DDL', raide, 'MATR_ASSE', repk=nume)
            lstock = .true.
        else
            call dismoi('NUME_DDL', modes, 'RESU_DYNA', repk=nume, arret='C',&
                        ier=iret)
        endif
    endif
!
!     --- CONTROLE PREALABLE ---
    do imode = 1, nbmode
        jmode = resufi(imode,1)
        if (jmode .lt. 1 .and. ineg .gt. 0) then
            call utmess('A', 'ALGELINE3_79')
        endif
    end do
!
!     --- STOCKAGE DES MODES ---
    call rsexis(modes, ier)
    if (ier .eq. 0) then
        call utmess('F', 'ALGELINE3_80')
    endif
!
    nmin = resufi(1,1)
    imin = 1
    nmax = resufi(1,1)
    imax = 1
    do imode = 2, nbmode
        if (resufi(imode,1) .lt. nmin) then
            nmin = resufi(imode,1)
            imin = imode
        endif
        if (resufi(imode,1) .gt. nmax) then
            nmax = resufi(imode,1)
            imax = imode
        endif
    end do
    nmin1 = nmax
!
!     ON RECUPERE LE NOM DE LA MATRICE DE RAIDEUR AFIN DE
!     DETERMINER LE NOM DU MODELE, DU MATERIAU ET DES
!     CARACTERISTIQUES ELEMENTAIRES
    if (lstock) then
        if (typcon(1:9) .eq. 'MODE_MECA' .or. typcon(1:9) .eq. 'MODE_ACOU' .or.&
            typcon(1:10) .eq. 'MODE_FLAMB' .or. typcon(1:9) .eq. 'MODE_STAB') then
            call dismoi('NOM_MODELE', raide, 'MATR_ASSE', repk=modele)
            call dismoi('CHAM_MATER', raide, 'MATR_ASSE', repk=chmat)
            call dismoi('CARA_ELEM', raide, 'MATR_ASSE', repk=carael)
        else if (typcon(1:9).eq.'MODE_GENE') then
            call jeveuo(raide(1:19)//'.LIME', 'L', jmodg)
            if (zk24(jmodg)(1:8) .eq. '        ') then
!            ON EST PASSE PAR UN PROJ_MATR_BASE
                call jeveuo(raide(1:19)//'.REFA', 'L', jmodg)
                basemo = zk24(jmodg)(1:8)
                call dismoi('REF_RIGI_PREM', basemo, 'RESU_DYNA', repk=raide2, arret='C',&
                            ier=iret)
                if (raide2 .eq. ' ') then
                    call jeveuo(jexnum(basemo//'           .TACH', 1), 'L', jmod2)
                    sd2 = zk24(jmod2)(1:8)
                    call rsadpa(sd2, 'L', 1, 'MODELE', 1,&
                                0, sjv=jpara, styp=k8b)
                    modele = zk8(jpara)
                    call rsadpa(sd2, 'L', 1, 'CHAMPMAT', 1,&
                                0, sjv=jpara, styp=k8b)
                    chmat = zk8(jpara)
                    call rsadpa(sd2, 'L', 1, 'CARAELEM', 1,&
                                0, sjv=jpara, styp=k8b)
                    carael = zk8(jpara)
                    goto 39
                else
                    call jeveuo(raide2(1:19)//'.LIME', 'L', jlime)
                    if (zk24(jlime)(1:8) .ne. '        ') then
!            ON EST PASSE PAR UN ASSE_MATRICE/CALC_MATR_ELEM
                        call jeexin(zk24(jlime)(1:8)//'      .MODG.SSME', iret)
                        if (iret .ne. 0) then
                            call jeveuo(zk24(jlime)(1:8)//'      .MODG.SSME', 'L', jmacr)
                            call jeveuo(zk8(jmacr)//'.MAEL_INER_REFE', 'L', jbasm)
                            basemo = zk24(jbasm)(1:8)
                            call dismoi('REF_RIGI_PREM', basemo, 'RESU_DYNA', repk=raide2)
                            call jeveuo(raide2(1:19)//'.LIME', 'L', jlime)
                            call jeveuo(zk24(jlime)(1:8)//'           .RERR', 'L', vk24=rerr)
                            modele = rerr(1)(1:8)
                            chmat = rerr(4)(1:8)
                            carael = rerr(5)(1:8)
                            goto 39
                        endif
                    endif
                endif
            else
!            ON EST PASSE PAR UN DEFI_MODELE_GENE
                call jeveuo(zk24(jmodg)(1:8)//'      .MODG.SSME', 'L', jmacr)
                call jeveuo(zk8(jmacr)//'.MAEL_INER_REFE', 'L', jbasm)
                basemo = zk24(jbasm)(1:8)
                call dismoi('REF_RIGI_PREM', basemo, 'RESU_DYNA', repk=raide2, arret='C',&
                            ier=iret)
                if (raide2 .eq. ' ') then
                    call jeveuo(jexnum(basemo//'           .TACH', 1), 'L', jmod2)
                    sd2 = zk24(jmod2)(1:8)
                    call rsadpa(sd2, 'L', 1, 'MODELE', 1,&
                                0, sjv=jpara, styp=k8b)
                    modele = zk8(jpara)
                    call rsadpa(sd2, 'L', 1, 'CHAMPMAT', 1,&
                                0, sjv=jpara, styp=k8b)
                    chmat = zk8(jpara)
                    call rsadpa(sd2, 'L', 1, 'CARAELEM', 1,&
                                0, sjv=jpara, styp=k8b)
                    carael = zk8(jpara)
                    goto 39
                endif
            endif
            call dismoi('NOM_MODELE', raide2(1:8), 'MATR_ASSE', repk=modele)
            call dismoi('CHAM_MATER', raide2(1:8), 'MATR_ASSE', repk=chmat)
            call dismoi('CARA_ELEM', raide2(1:8), 'MATR_ASSE', repk=carael)
        endif
    endif
!
 39 continue
!
    do imode = 1, nbmode
!
!       STOCKAGE DES FREQUENCES PAR ORDRE CROISSANT DE NUMERO
        if (imode .eq. 1) then
            kmode = imin
        else if (imode.eq.nbmode) then
            kmode = imax
        else
            do lmode = 1, nbmode
                if (resufi(lmode,1) .gt. nmin .and. resufi(lmode,1) .lt. nmin1) then
                    nmin1 = resufi(lmode,1)
                    kmode = lmode
                endif
            end do
            nmin = nmin1
            nmin1 = nmax
        endif
!
        jmode = resufi(kmode,1)
        nordr = iprec + imode
!
!        --- VECTEUR PROPRE ---
        call rsexch(' ', modes, nosy, nordr, chamno,&
                    ier)
        if (ier .eq. 0) then
        else if (ier .eq. 100 .and. lrefd) then
            call vtcreb(chamno, nume, 'G', type(1:1), neq)
        else
            vali (1) = kmode
            vali (2) = jmode
            vali (3) = ier
            valk = chamno
            call utmess('F', 'ALGELINE4_85', sk=valk, ni=3, vali=vali)
        endif
        if (typcon .eq. 'MODE_GENE' .or. typcon .eq. 'HARM_GENE') then
            call jeecra(chamno//'.DESC', 'DOCU', iarg, 'VGEN')
! GLUTE CAR ON A UTILISE VTCRE[ABM] POUR UN CHAM_GENE QUI A UN .REFE
! DE TAILLE 2 ET NON 4 COMME UN CHAM_NO
            call juveca(chamno//'.REFE', 2)
        endif
        call jeveuo(chamno//'.VALE', 'E', lvale)
        if (type(1:1) .eq. 'R') then
            call dcopy(neq, vecpr8(1, kmode), 1, zr(lvale), 1)
        else if (type(1:1) .eq. 'C') then
            call zcopy(neq, vecpc8(1, kmode), 1, zc(lvale), 1)
        endif
!       SI LE CHAMP A DEJA ETE NOTE PAR SEMOCO, ON NE LE REFAIT PAS
        if (ier .ne. 0) call rsnoch(modes, nosy, nordr)
!
! ----- ON STOCKE 'NUME_MODE'
!
        irang = indk24(nopara,nopast(1),1,nbpari)
        if (irang .gt. 0) then
            call rsadpa(modes, 'E', 1, nopast(1), nordr,&
                        0, sjv=ladpa, styp=k8b)
            zi(ladpa) = resufi(kmode,irang)
        endif
!
! ----- ON STOCKE 'NORME'
!
        irang = indk24(nopara(nbpari+1),nopast(2),1,nbpark)
        if (irang .gt. 0) then
            call rsadpa(modes, 'E', 1, nopast(2), nordr,&
                        0, sjv=ladpa, styp=k8b)
            zk24(ladpa) = resufk(kmode,irang)
        endif
!
! ----- ON STOCKE 'TYPE_MODE' POUR LES MODES PROPRES 'MODE_MECA'
!
        if (typcon(1:9) .eq. 'MODE_MECA' .or. typcon(1:9) .eq. 'MODE_GENE') then
!
            irang = indk24(nopara(nbpari+1),nopast(3),1,nbpark)
            if (irang .gt. 0) then
                typmod = resufk(kmode,irang)
                if (typmod(1:8) .eq. '        ') then
                    typmod = 'MODE_DYN'
                endif
                call rsadpa(modes, 'E', 1, nopast(3), nordr,&
                            0, sjv=ladpa, styp=k8b)
                zk16(ladpa)= typmod
            endif
!
        endif
!
! ----- ON STOCKE 'NOEUD_CMP'
!
        irang = indk24(nopara(nbpari+1),nopast(4),1,nbpark)
        if (irang .gt. 0) then
            call rsadpa(modes, 'E', 1, nopast(4), nordr,&
                        0, sjv=ladpa, styp=k8b)
            zk16(ladpa) = resufk(kmode,irang)
        endif
!
! ----- ON STOCKE : MODELE, CARA_ELEM, CHAM_MATER
!
        if (lstock) then
            call rsadpa(modes, 'E', 1, 'MODELE', nordr,&
                        0, sjv=ladpa, styp=k8b)
            zk8(ladpa)=modele
            call rsadpa(modes, 'E', 1, 'CHAMPMAT', nordr,&
                        0, sjv=ladpa, styp=k8b)
            zk8(ladpa)=chmat
            call rsadpa(modes, 'E', 1, 'CARAELEM', nordr,&
                        0, sjv=ladpa, styp=k8b)
            zk8(ladpa)=carael
        endif
!
!
! ----- ON STOCKE LES PARAMETRES REELS
!
        if (typcon .eq. 'MODE_FLAMB') then
            call rsadpa(modes, 'E', 1, 'CHAR_CRIT', nordr,&
                        0, sjv=ladpa, styp=k8b)
            if (nomcmd .eq. 'NORM_MODE') then
                zr(ladpa) = resufr(kmode,1)
            else
                zr(ladpa) = resufr(kmode,2)
            endif
        else if (typcon .eq. 'MODE_STAB') then
            call rsadpa(modes, 'E', 1, 'CHAR_STAB', 1,&
                        0, sjv=ladpa, styp=k8b)
            zr(ladpa) = resufr(kmode,1)
        else
            do i = 5, nbpast
                irang = indk24(nopara(nbpari+nbpark+1),nopast(i),1, nbparr)
                if (irang .gt. 0) then
                    call rsadpa(modes, 'E', 1, nopast(i), nordr,&
                                0, sjv=ladpa, styp=k8b)
                    zr(ladpa) = resufr(kmode,irang)
                endif
            end do
        endif
!
    end do
!
    call jedema()
end subroutine
