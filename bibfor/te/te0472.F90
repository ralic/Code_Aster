subroutine te0472(option, nomte)
    implicit none
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/borthm.h"
#include "asterfort/elrefe_info.h"
#include "asterfort/fointe.h"
#include "asterfort/jevech.h"
#include "asterfort/vff2dn.h"
#include "asterfort/lteatt.h"
    character(len=16) :: option, nomte
! ======================================================================
! person_in_charge: sylvie.granet at edf.fr
! ======================================================================
! COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
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
!
!     BUT: CALCUL DES VECTEURS ELEMENTAIRES EN MECANIQUE
!          CORRESPONDANT A UN FLUX THM (THH, THHM, THH, THH2,HHM,HM,HH)
!          SUR DES FACES D'ELEMENTS ISOPARAMETRIQUES 2D
!
!          OPTION : 'CHAR_MECA_FLUX_R'
!          OPTION : 'CHAR_MECA_FLUX_F'
!
!    - ARGUMENTS:
!        DONNEES:      OPTION       -->  OPTION DE CALCUL
!                      NOMTE        -->  NOM DU TYPE ELEMENT
! ======================================================================
! NNO      NB DE NOEUDS DE L'ELEMENT DE BORD QUADRATIQUE
! NNO2     NB DE NOEUDS DE L'ELEMENT DE BORD LINEAIRE
! NNOS     NB DE NOEUDS EXTREMITE
! NDLNO    NB DE DDL DES NOEUDS EXTREMITE
! NDLNM    NB DE DDL DES NOEUDS MILIEUX
! NPG      NB DE POINTS DE GAUSS DE L'ELEMENT DE BORD
! ======================================================================
! ======================================================================
    aster_logical :: axi, perman, vf
    integer :: nno, nno2, nnos, kp, npg, ndim, jgano, jgano2, napre1, napre2
    integer :: ipoids, ipoid2, ivf, ivf2, idfde, idfde2, igeom, natemp
    integer :: ipres, k, kk, i, l, ires, iflux, itemps, iopt, ipresf, ndlnm
    integer :: ifluxf, iret, ndlno
    real(kind=8) :: poids, r, z, tx, ty, nx, ny, valpar(3), deltat, tplus
    real(kind=8) :: pres, presf, poids2, nx2, ny2, flu1, flu2, fluth
    character(len=8) :: nompar(3), typmod(2)
    integer :: typvf
! ======================================================================
! --- CARACTERISTIQUES DE LA MODELISATION ------------------------------
! ======================================================================
    call borthm(axi, vf, perman, typvf, typmod,&
                ndim, ndlno, ndlnm)
! ======================================================================
! --- DEFINITION DE L'ELEMENT (NOEUDS, SOMMETS, POINTS DE GAUSS) -------
! ======================================================================
! --- LES DDLS THERMO-HYDRAULIQUES NE SONT PLUS EXPRIMES AUX NOEUDS ----
! --- MILIEUX ----------------------------------------------------------
! ======================================================================
! --- INTERPOLATION (QUADRATIQUE) POUR LA MECANIQUE --------------------
! ======================================================================
    call elrefe_info(fami='RIGI', ndim=ndim, nno=nno, nnos=nnos, npg=npg,&
                     jpoids=ipoids, jvf=ivf, jdfde=idfde, jgano=jgano)
! ======================================================================
! --- INTERPOLATION (LINEAIRE) POUR LA THERMO-HYDRAULIQUE --------------
! ======================================================================
    call elrefe_info(elrefe='SE2', fami='RIGI', ndim=ndim, nno=nno2, nnos=nnos,&
                     npg=npg, jpoids=ipoid2, jvf=ivf2, jdfde=idfde2, jgano=jgano2)
! ======================================================================
! --- RECUPERATION DES CHAMPS IN ET DES CHAMPS OUT ---------------------
! ======================================================================
    call jevech('PGEOMER', 'L', igeom)
    call jevech('PVECTUR', 'E', ires)
! ======================================================================
! --- CAS DES FLUX -----------------------------------------------------
! ======================================================================
    if (option .eq. 'CHAR_MECA_FLUX_R') then
        iopt = 1
        call jevech('PFLUXR', 'L', iflux)
        call jevech('PTEMPSR', 'L', itemps)
        deltat = zr(itemps+1)
    else if (option.eq.'CHAR_MECA_FLUX_F') then
        iopt = 2
        call jevech('PFLUXF', 'L', ifluxf)
        call jevech('PTEMPSR', 'L', itemps)
        tplus = zr(itemps)
        deltat = zr(itemps+1)
        nompar(1) = 'X'
        nompar(2) = 'Y'
        nompar(3) = 'INST'
        valpar(3) = tplus
! ======================================================================
! --- CAS DES PRESSIONS MECANIQUES -------------------------------------
! ======================================================================
    else if (option.eq.'CHAR_MECA_PRES_R') then
        iopt = 3
        call jevech('PPRESSR', 'L', ipres)
    else if (option.eq.'CHAR_MECA_PRES_F') then
        iopt = 4
        call jevech('PPRESSF', 'L', ipresf)
        call jevech('PTEMPSR', 'L', itemps)
        nompar(1) = 'X'
        nompar(2) = 'Y'
        nompar(3) = 'INST'
        valpar(3) = zr(itemps)
    endif
! ======================================================================
! --- CAS DU PERMANENT POUR LA PARTIE H OU T : LE SYSTEME A ETE --------
! --- CONSTRUIT EN SIMPLIFIANT PAR LE PAS DE TEMPS. ON DOIT DONC -------
! --- LE PRENDRE EGAL A 1 DANS LE CALCUL DU SECOND MEMBRE --------------
! ======================================================================
! ======================================================================
! --- BOUCLE SUR LES POINTS DE GAUSS DE L'ELEMENT DE BORD --------------
! ======================================================================
    do 190 kp = 1, npg
        k = (kp-1)*nno
        kk = (kp-1)*nno2
! ======================================================================
! --- RECUPERATION DES DERIVEES DES FONCTONS DE FORMES -----------------
! ======================================================================
        call vff2dn(ndim, nno, kp, ipoids, idfde,&
                    zr(igeom), nx, ny, poids)
        call vff2dn(ndim, nno2, kp, ipoid2, idfde2,&
                    zr(igeom), nx2, ny2, poids2)
! ======================================================================
! --- MODIFICATION DU POIDS POUR LES CAS AXI ---------------------------
! ======================================================================
        if (axi) then
            r = 0.d0
            z = 0.d0
            do 10 i = 1, nno
                l = (kp-1)*nno + i
                r = r + zr(igeom+2*i-2)*zr(ivf+l-1)
 10         continue
            poids = poids*r
        endif
! ======================================================================
! --- OPTION CHAR_MECA_FLUX_R OU CHAR_MECA_FLUX_F ----------------------
! ======================================================================
! --- DANS CES CAS LES INTERPOLATIONS SONT LINEAIRES -------------------
! --- CAR FLUX = GRANDEURS TH (ON UTILISE IVF2 ET NNO2) ----------------
! ======================================================================
! --- NAPRE1,NAPRE2,NATEMP SONT MIS EN PLACE ---------------------------
! --- POUR UNE EVENTUELLE MODIFICATION DE L'ORDRE DES DDL --------------
! --- PRE1, PRE2, TEMP DANS LES CATALOGUES D'ELEMENTS ------------------
! ======================================================================
! --- FLUTH REPRESENTE LE FLUX THERMIQUE -------------------------------
! --- FLU1 REPRESENTE LE FLUX ASSOCIE A PRE1 ---------------------------
! --- FLU2 REPRESENTE LE FLUX ASSOCIE A PRE2 ---------------------------
! ======================================================================
        if (iopt .eq. 1 .or. iopt .eq. 2) then
! ======================================================================
! --- SI MODELISATION = THHM, THH OU THH2 ------------------------------
! ======================================================================
            if (lteatt('MODTHM','THHM') .or. lteatt('MODTHM','THH') .or.&
                lteatt('MODTHM','THH2')) then
                napre1 = 0
                napre2 = 1
                natemp = 2
                if (iopt .eq. 1) then
                    flu1 = zr(iflux+(kp-1)*3+napre1)
                    flu2 = zr(iflux+(kp-1)*3+napre2)
                    fluth = zr(iflux+(kp-1)*3+natemp)
                else if (iopt.eq.2) then
                    r = 0.d0
                    z = 0.d0
                    do 20 i = 1, nno2
                        l = (kp-1)*nno2 + i
                        r = r + zr(igeom+2*i-2)*zr(ivf2+l-1)
                        z = z + zr(igeom+2*i-1)*zr(ivf2+l-1)
 20                 continue
                    valpar(1) = r
                    valpar(2) = z
                    call fointe('FM', zk8(ifluxf+napre1), 3, nompar, valpar,&
                                flu1, iret)
                    call fointe('FM', zk8(ifluxf+napre2), 3, nompar, valpar,&
                                flu2, iret)
                    call fointe('FM', zk8(ifluxf+natemp), 3, nompar, valpar,&
                                fluth, iret)
                endif
! ======================================================================
! --- SI MODELISATION = THHM, OU THH2M ---------------------------------
! ======================================================================
                if (lteatt('MODTHM','THHM') .or. lteatt('MODTHM','THH2M')) then
                    do 30 i = 1, nno2
                        l = 5* (i-1) - 1
                        zr(ires+l+3) = zr(ires+l+3) - poids*deltat* flu1*zr(ivf2+kk+i-1)
                        zr(ires+l+4) = zr(ires+l+4) - poids*deltat* flu2*zr(ivf2+kk+i-1)
                        zr(ires+l+5) = zr(ires+l+5) - poids*deltat* fluth*zr(ivf2+kk+i-1)
 30                 continue
                else
                    do 40 i = 1, nno2
                        l = 3* (i-1) - 1
                        zr(ires+l+1) = zr(ires+l+1) - poids*deltat* flu1*zr(ivf2+kk+i-1)
                        zr(ires+l+2) = zr(ires+l+2) - poids*deltat* flu2*zr(ivf2+kk+i-1)
                        zr(ires+l+3) = zr(ires+l+3) - poids*deltat* fluth*zr(ivf2+kk+i-1)
 40                 continue
                endif
            endif
! ======================================================================
! --- SI MODELISATION = HH, OU HH2 -------------------------------------
! ======================================================================
            if (lteatt('MODTHM','HH') .or. lteatt('MODTHM','HH2') .or.&
                lteatt('MODTHM','SUSHI')) then
                napre1 = 0
                napre2 = 1
                if (iopt .eq. 1) then
                    flu1 = zr(iflux+(kp-1)*2+napre1)
                    flu2 = zr(iflux+(kp-1)*2+napre2)
                else if (iopt.eq.2) then
                    r = 0.d0
                    z = 0.d0
                    do 201 i = 1, nno2
                        l = (kp-1)*nno2 + i
                        r = r + zr(igeom+2*i-2)*zr(ivf2+l-1)
                        z = z + zr(igeom+2*i-1)*zr(ivf2+l-1)
201                 continue
                    valpar(1) = r
                    valpar(2) = z
                    call fointe('FM', zk8(ifluxf+napre1), 2, nompar, valpar,&
                                flu1, iret)
                    call fointe('FM', zk8(ifluxf+napre2), 2, nompar, valpar,&
                                flu2, iret)
                endif
!
                if (lteatt('MODTHM','SUSHI')) then
! ======================================================================
! --- SI TE = DHH2S3_SU ------------------------------------------------
! ======================================================================
                    do 401 i = 1, nno2
                        zr(ires) = zr(ires) - poids*flu1*zr(ivf2+kk+i- 1)
                        zr(ires+1) = zr(ires+1) - poids*flu2*zr(ivf2+ kk+i-1)
401                 continue
                else
! ======================================================================
! --- SI MODELISATION = HH*, OU HH2*------------------------------------
! ======================================================================
                    do 402 i = 1, nno2
                        l = 2* (i-1) - 1
                        zr(ires+l+1) = zr(ires+l+1) - poids*deltat* flu1*zr(ivf2+kk+i-1)
                        zr(ires+l+2) = zr(ires+l+2) - poids*deltat* flu2*zr(ivf2+kk+i-1)
402                 continue
                endif
            endif
! ======================================================================
! --- SI MODELISATION = THV --------------------------------------------
! ======================================================================
            if (lteatt('MODTHM','THV')) then
                napre1 = 0
                natemp = 1
                if (iopt .eq. 1) then
                    flu1 = zr(iflux+ (kp-1)*2+napre1)
                    fluth = zr(iflux+ (kp-1)*2+natemp)
                else if (iopt.eq.2) then
                    r = 0.d0
                    z = 0.d0
                    do 50 i = 1, nno2
                        l = (kp-1)*nno2 + i
                        r = r + zr(igeom+2*i-2)*zr(ivf2+l-1)
                        z = z + zr(igeom+2*i-1)*zr(ivf2+l-1)
 50                 continue
                    valpar(1) = r
                    valpar(2) = z
                    call fointe('FM', zk8(ifluxf+napre1), 3, nompar, valpar,&
                                flu1, iret)
                    call fointe('FM', zk8(ifluxf+napre2), 3, nompar, valpar,&
                                flu2, iret)
                    call fointe('FM', zk8(ifluxf+natemp), 3, nompar, valpar,&
                                fluth, iret)
                endif
                do 60 i = 1, nno2
                    l = 2* (i-1) - 1
                    zr(ires+l+1) = zr(ires+l+1) - poids*deltat*flu1* zr(ivf2+kk+i-1)
                    zr(ires+l+2) = zr(ires+l+2) - poids*deltat*fluth* zr(ivf2+kk+i-1)
 60             continue
            endif
! ======================================================================
! --- SI MODELISATION = H ---------------------------------------------
! ======================================================================
            if (lteatt('CODMOD','DHA')) then
                napre1 = 0
                if (iopt .eq. 1) then
                    flu1 = zr(iflux+ (kp-1)+napre1)
                else if (iopt.eq.2) then
                    r = 0.d0
                    z = 0.d0
                    do 69 i = 1, nno2
                        l = (kp-1)*nno2 + i
                        r = r + zr(igeom+2*i-2)*zr(ivf2+l-1)
                        z = z + zr(igeom+2*i-1)*zr(ivf2+l-1)
 69                 continue
                    valpar(1) = r
                    valpar(2) = z
                    call fointe('FM', zk8(ifluxf+napre1), 3, nompar, valpar,&
                                flu1, iret)
                endif
                do 79 i = 1, nno2
                    l = 1* (i-1) - 1
                    zr(ires+l+1) = zr(ires+l+1) - poids*deltat*flu1* zr(ivf2+kk+i-1)
 79             continue
            endif
! ======================================================================
! --- SI MODELISATION = HM ---------------------------------------------
! ======================================================================
            if (lteatt('MODTHM','HM')) then
                napre1 = 0
                if (iopt .eq. 1) then
                    flu1 = zr(iflux+ (kp-1)+napre1)
                else if (iopt.eq.2) then
                    r = 0.d0
                    z = 0.d0
                    do 70 i = 1, nno2
                        l = (kp-1)*nno2 + i
                        r = r + zr(igeom+2*i-2)*zr(ivf2+l-1)
                        z = z + zr(igeom+2*i-1)*zr(ivf2+l-1)
 70                 continue
                    valpar(1) = r
                    valpar(2) = z
                    call fointe('FM', zk8(ifluxf+napre1), 3, nompar, valpar,&
                                flu1, iret)
                endif
                do 80 i = 1, nno2
                    l = 3* (i-1) - 1
                    if (.not.perman) then
                        zr(ires+l+3) = zr(ires+l+3) - poids*deltat* flu1*zr(ivf2+kk+i-1)
                    else
                        zr(ires+l+3) = zr(ires+l+3) - poids*flu1*zr( ivf2+kk+i-1)
                    endif
 80             continue
            endif
! ======================================================================
! --- SI MODELISATION = HHM OU HH2M ------------------------------------
! ======================================================================
            if (lteatt('MODTHM','HHM') .or. lteatt('MODTHM','HH2M')) then
                napre1 = 0
                napre2 = 1
                if (iopt .eq. 1) then
                    flu1 = zr(iflux+ (kp-1)*2+napre1)
                    flu2 = zr(iflux+ (kp-1)*2+napre2)
                else if (iopt.eq.2) then
                    r = 0.d0
                    z = 0.d0
                    do 90 i = 1, nno2
                        l = (kp-1)*nno2 + i
                        r = r + zr(igeom+2*i-2)*zr(ivf2+l-1)
                        z = z + zr(igeom+2*i-1)*zr(ivf2+l-1)
 90                 continue
                    valpar(1) = r
                    valpar(2) = z
                    call fointe('FM', zk8(ifluxf+napre1), 3, nompar, valpar,&
                                flu1, iret)
                    call fointe('FM', zk8(ifluxf+napre2), 3, nompar, valpar,&
                                flu2, iret)
                endif
                do 100 i = 1, nno2
                    l = 4* (i-1) - 1
                    zr(ires+l+3) = zr(ires+l+3) - poids*deltat*flu1* zr(ivf2+kk+i-1)
                    zr(ires+l+4) = zr(ires+l+4) - poids*deltat*flu2* zr(ivf2+kk+i-1)
100             continue
            endif
! ======================================================================
! --- SI MODELISATION = THM --------------------------------------------
! ======================================================================
            if (lteatt('MODTHM','THM')) then
                napre1 = 0
                natemp = 1
                if (iopt .eq. 1) then
                    flu1 = zr(iflux+ (kp-1)*2+napre1)
                    fluth = zr(iflux+ (kp-1)*2+natemp)
                else if (iopt.eq.2) then
                    r = 0.d0
                    z = 0.d0
                    do 110 i = 1, nno2
                        l = (kp-1)*nno2 + i
                        r = r + zr(igeom+2*i-2)*zr(ivf2+l-1)
                        z = z + zr(igeom+2*i-1)*zr(ivf2+l-1)
110                 continue
                    valpar(1) = r
                    valpar(2) = z
                    call fointe('FM', zk8(ifluxf+napre1), 3, nompar, valpar,&
                                flu1, iret)
                    call fointe('FM', zk8(ifluxf+natemp), 3, nompar, valpar,&
                                fluth, iret)
                endif
                do 120 i = 1, nno2
                    l = 4* (i-1) - 1
                    zr(ires+l+3) = zr(ires+l+3) - poids*deltat*flu1* zr(ivf2+kk+i-1)
                    zr(ires+l+4) = zr(ires+l+4) - poids*deltat*fluth* zr(ivf2+kk+i-1)
120             continue
            endif
! ======================================================================
! --- OPTION CHAR_MECA_PRES_R OU CHAR_MECA_PRES_F ----------------------
! ======================================================================
! --- ICI, LES INTERPOLATIONS SONT QUADRATIQUES ------------------------
! ======================================================================
        else if ((iopt.eq.3) .or. (iopt.eq.4)) then
            if (iopt .eq. 3) then
                pres = 0.d0
                do 160 i = 1, nno
                    l = (kp-1)*nno + i
                    pres = pres + zr(ipres+i-1)*zr(ivf+l-1)
160             continue
            else if (iopt.eq.4) then
                pres = 0.d0
                do 170 i = 1, nno
                    valpar(1) = zr(igeom+2*i-2)
                    valpar(2) = zr(igeom+2*i-1)
                    call fointe('FM', zk8(ipresf), 3, nompar, valpar,&
                                presf, iret)
                    l = (kp-1)*nno + i
                    pres = pres + presf*zr(ivf+l-1)
170             continue
            endif
            tx = -nx*pres
            ty = -ny*pres
            do 180 i = 1, nnos
                l = ndlno* (i-1) - 1
                zr(ires+l+1) = zr(ires+l+1) + tx*zr(ivf+k+i-1)*poids
                zr(ires+l+2) = zr(ires+l+2) + ty*zr(ivf+k+i-1)*poids
180         continue
            do 181 i = 1, (nno - nnos)
                l = ndlno*nnos+ndlnm* (i-1) -1
                zr(ires+l+1) = zr(ires+l+1) + tx*zr(ivf+k+i+nnos-1)* poids
                zr(ires+l+2) = zr(ires+l+2) + ty*zr(ivf+k+i+nnos-1)* poids
181         continue
        endif
190 end do
! ======================================================================
end subroutine
