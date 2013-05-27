subroutine tfveri(nommcf, nbocc, itypfl)
!-----------------------------------------------------------------------
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!-----------------------------------------------------------------------
    implicit none
!-----------------------------------------------------------------------
!     VERIFICATIONS DE PREMIER NIVEAU
!     APPELANT : OP0143 , OPERATEUR DEFI_FLUI_STRU
!-----------------------------------------------------------------------
!  IN   : NOMMCF : NOM DU MOT-CLE FACTEUR UTILISE
!  IN   : NBOCC  : NOMBRE D'OCCURENCES DU MOT-CLE FACTEUR UTILISE
!  IN   : ITYPFL : INDICE CARACTERISTIQUE DE LA CONFIGURATION ETUDIEE
!-----------------------------------------------------------------------
    include 'asterc/getvid.h'
    include 'asterc/getvis.h'
    include 'asterc/getvr8.h'
    include 'asterc/getvtx.h'
    include 'asterfort/tfvegr.h'
    include 'asterfort/u2mess.h'
    integer :: itypfl
    character(len=16) :: nommcf
! ----------------------------------------------------------------------
    integer :: count1, count2, count3, count4, count5
    integer :: ocgril
    character(len=2) :: carapa(4)
    character(len=3) :: ouinon
    character(len=8) :: k8bid
    character(len=9) :: tpas
!      CHARACTER*9   TYPAS(2)
    real(kind=8) :: vect(3), valepa(4)
    integer :: iarg
!
!      DATA TYPAS   /'CARRE_LIGN ','TRIA_LIGN'/
! ----------------------------------------------------------------------
!
!
!
! ----1.CAS D'UN FAISCEAU_TRANS
!       -----------------------
!
!-----------------------------------------------------------------------
    integer :: iangl, ibid, icapa, icara, icm, icmp, icoup
    integer :: ier2, igra2, ihy, ihz, iocc, ipas, ipesan
    integer :: ir, irayon, irho, irhoe, irhoi, irugo, itpas
    integer :: itres, ivapa, ivect, ivisc, jcoup, nbcoor, nbhy
    integer :: nbhz, nbocc, nbr, nbtub, nbtub2, nbyc, nbzc
    integer :: ncara, ncm, ncmp, ncoup, npas, nrhoe, nrhoi
    integer :: ntpas, ntres, ntypg
    real(kind=8) :: rbid
!-----------------------------------------------------------------------
    if (itypfl .eq. 1) then
! ---    VERIFICATION DE LA PRESENCE D AU MOINS UNE OCCURENCE DU
!        MOT-CLE COUPLAGE
        ncoup = 0
        do 10 iocc = 1, nbocc
            call getvtx(nommcf, 'COUPLAGE', iocc, iarg, 0,&
                        k8bid, icoup)
            if (icoup .ne. 0) then
                ncoup = ncoup + 1
                jcoup = iocc
            endif
10      continue
        if (ncoup .eq. 0) then
            call u2mess('E', 'MODELISA7_19')
            goto 9999
        endif
        ncara = 0
        nrhoi = 0
        nrhoe = 0
        ncmp = 0
        do 20 iocc = 1, nbocc
            call getvid(nommcf, 'CARA_ELEM', iocc, iarg, 0,&
                        k8bid, icara)
            if (icara .ne. 0) ncara = ncara + 1
            call getvid(nommcf, 'PROF_RHO_F_INT', iocc, iarg, 0,&
                        k8bid, irhoi)
            if (irhoi .ne. 0) nrhoi = nrhoi + 1
            call getvid(nommcf, 'PROF_RHO_F_EXT', iocc, iarg, 0,&
                        k8bid, irhoe)
            if (irhoe .ne. 0) nrhoe = nrhoe + 1
            call getvtx(nommcf, 'NOM_CMP       ', iocc, iarg, 0,&
                        k8bid, icmp)
            if (icmp .ne. 0) ncmp = ncmp + 1
20      continue
!
        call getvtx(nommcf, 'COUPLAGE', jcoup, iarg, 1,&
                    ouinon, ibid)
!
! -------1.1.SI PRISE EN COMPTE DU COUPLAGE
!
        if (ouinon .eq. 'OUI') then
            ntpas = 0
            ntres = 0
            npas = 0
            ncm = 0
            do 30 iocc = 1, nbocc
                call getvtx(nommcf, 'TYPE_PAS', iocc, iarg, 0,&
                            tpas, itpas)
                if (itpas .ne. 0) then
                    ntpas = ntpas + 1
                endif
                call getvis(nommcf, 'TYPE_RESEAU', iocc, iarg, 0,&
                            ibid, itres)
                if (itres .ne. 0) then
                    ntres = ntres + 1
                endif
                call getvr8(nommcf, 'PAS', iocc, iarg, 0,&
                            rbid, ipas)
                if (ipas .ne. 0) then
!                  JPAS = IOCC
                    npas = npas + 1
                endif
30          continue
            if (ntpas .eq. 0 .or. ntres .ne. nbocc .or. npas .eq. 0) then
                call u2mess('E', 'MODELISA7_20')
            endif
!
! ------1.2.SI NON PRISE EN COMPTE DU COUPLAGE
!
        else
            ncm = 0
            do 50 iocc = 1, nbocc
                call getvr8(nommcf, 'COEF_MASS_AJOU', iocc, iarg, 0,&
                            rbid, icm)
                if (icm .ne. 0) then
                    ncm = ncm + 1
                endif
50          continue
            if (ncm .eq. 0) then
                call u2mess('E', 'MODELISA7_21')
            endif
        endif
!
! ------1.3.VERIFICATION DE LA PRESENCE  DES MOT-CLE DEVANT APPARAITRE
!       AU MOINS UNE FOIS DANS L UNE DES OCCURENCES DU MOT-CLE FACTEUR
!
        if (ncara .eq. 0) then
            call u2mess('E', 'MODELISA7_22')
        endif
        if (nrhoi .eq. 0) then
            call u2mess('E', 'MODELISA7_23')
        endif
        if (nrhoe .eq. 0) then
            call u2mess('E', 'MODELISA7_24')
        endif
        if (ncara .eq. 0) then
            call u2mess('E', 'MODELISA7_25')
        endif
!
! ----2.CAS D'UNE GRAPPE
!       ----------------
!
    else if (itypfl.eq.2) then
!
        call getvtx(nommcf, 'COUPLAGE', 1, iarg, 1,&
                    ouinon, ibid)
        if (ouinon .eq. 'OUI') then
            call getvtx(nommcf, 'GRAPPE_2', 1, iarg, 0,&
                        k8bid, igra2)
            if (igra2 .eq. 0) then
                call u2mess('E', 'MODELISA7_26')
            endif
        endif
!
!
! ----3.CAS D'UN FAISCEAU_AXIAL
!       -----------------------
!
    else if (itypfl.eq.3) then
!
        count1 = 0
        count2 = 0
        count3 = 0
        count4 = 0
        count5 = 0
        ocgril = 0
!
        do 80 iocc = 1, nbocc
!
! --------3.1.SI PLUSIEURS OCCURENCES <RAYON_TUBE> ET <COOR_TUBE>
! --------    OBLIGATOIRES A CHAQUE OCCURENCE
! --------    VERIFICATION DES DONNEES POUR <COOR_TUBE>
!
            call getvr8(nommcf, 'RAYON_TUBE', iocc, iarg, 0,&
                        rbid, irayon)
            if (irayon .eq. 0) then
                if (nbocc .gt. 1) then
                    call u2mess('E', 'MODELISA7_27')
                endif
            else
                call getvr8(nommcf, 'COOR_TUBE', iocc, iarg, 0,&
                            rbid, nbcoor)
                nbcoor = abs(nbcoor)
                nbtub = int(nbcoor/2)
                nbtub2 = 2*nbtub
                if (nbtub2 .ne. nbcoor) then
                    call u2mess('E', 'MODELISA7_28')
                endif
            endif
!
! --------3.2.INCREMENTATION DU COMPTEUR POUR <VECT_X> ET VERIFICATION
! --------    DES DONNEES SI PRESENCE
!
            call getvr8(nommcf, 'VECT_X', iocc, iarg, 0,&
                        rbid, ivect)
            if (ivect .ne. 0) then
                count1 = count1 + 1
                if (abs(ivect) .ne. 3) then
                    call u2mess('E', 'MODELISA7_29')
                else
                    ier2 = 0
                    call getvr8(nommcf, 'VECT_X', iocc, iarg, 3,&
                                vect(1), ibid)
                    if (vect(1) .eq. 1.d0) then
                        if (vect(2) .ne. 0.d0 .or. vect(3) .ne. 0.d0) ier2 = 1
                    else if (vect(2).eq.1.d0) then
                        if (vect(1) .ne. 0.d0 .or. vect(3) .ne. 0.d0) ier2 = 1
                    else if (vect(3).eq.1.d0) then
                        if (vect(1) .ne. 0.d0 .or. vect(2) .ne. 0.d0) ier2 = 1
                    else
                        ier2 = 1
                    endif
                    if (ier2 .eq. 1) call u2mess('E', 'MODELISA7_30')
                endif
            endif
!
! --------3.3.INCREMENTATION DES COMPTEURS POUR <PROF_RHO_FLUI>,
! --------    <PROF_VISC_CINE> ET <RUGO_TUBE>
!
            call getvid(nommcf, 'PROF_RHO_FLUI', iocc, iarg, 0,&
                        k8bid, irho)
            if (irho .ne. 0) count2 = count2 + 1
!
            call getvid(nommcf, 'PROF_VISC_CINE', iocc, iarg, 0,&
                        k8bid, ivisc)
            if (ivisc .ne. 0) count3 = count3 + 1
!
            call getvr8(nommcf, 'RUGO_TUBE', iocc, iarg, 0,&
                        rbid, irugo)
            if (irugo .ne. 0) count4 = count4 + 1
!
! --------3.4.VERIFICATION DES DONNEES POUR <PESANTEUR> SI PRESENCE
!
            call getvr8(nommcf, 'PESANTEUR', iocc, iarg, 0,&
                        rbid, ipesan)
            ipesan = abs(ipesan)
            if (ipesan .ne. 0 .and. ipesan .ne. 4) then
                call u2mess('E', 'MODELISA7_31')
            endif
!
! --------3.5.INCREMENTATION DU COMPTEUR POUR <CARA_PAROI>
! --------    VERIFICATION DES DONNEES POUR <CARA_PAROI>, <VALE_PAROI>
! --------    ET <ANGL_VRIL> SI PRESENCE
!
            call getvtx(nommcf, 'CARA_PAROI', iocc, iarg, 0,&
                        k8bid, icapa)
            icapa = abs(icapa)
            if (icapa .ne. 0) then
                count5 = count5 + 1
                if (icapa .ne. 3 .and. icapa .ne. 4) then
                    call u2mess('E', 'MODELISA7_32')
                else
                    call getvr8(nommcf, 'VALE_PAROI', iocc, iarg, 0,&
                                rbid, ivapa)
                    ivapa = abs(ivapa)
                    if (ivapa .ne. icapa) then
                        call u2mess('E', 'MODELISA7_33')
                    else
                        call getvtx(nommcf, 'CARA_PAROI', iocc, iarg, icapa,&
                                    carapa(1), ibid)
                        call getvr8(nommcf, 'VALE_PAROI', iocc, iarg, ivapa,&
                                    valepa(1), ibid)
                        nbyc = 0
                        nbzc = 0
                        nbr = 0
                        nbhy = 0
                        nbhz = 0
                        if (icapa .eq. 3) then
                            do 60 icara = 1, icapa
                                if (carapa(icara) .eq. 'YC') nbyc = nbyc + 1
                                if (carapa(icara) .eq. 'ZC') nbzc = nbzc + 1
                                if (carapa(icara)(1:1) .eq. 'R') then
                                    nbr = nbr + 1
                                    ir = icara
                                endif
60                          continue
                            if (nbyc .ne. 1 .or. nbzc .ne. 1 .or. nbr .ne. 1) then
                                call u2mess('E', 'MODELISA7_34')
                            else if (valepa(ir).le.0.d0) then
                                call u2mess('E', 'MODELISA7_35')
                            endif
                        else
                            do 70 icara = 1, icapa
                                if (carapa(icara) .eq. 'YC') nbyc = nbyc + 1
                                if (carapa(icara) .eq. 'ZC') nbzc = nbzc + 1
                                if (carapa(icara) .eq. 'HY') then
                                    nbhy = nbhy + 1
                                    ihy = icara
                                endif
                                if (carapa(icara) .eq. 'HZ') then
                                    nbhz = nbhz + 1
                                    ihz = icara
                                endif
70                          continue
                            if (nbyc .ne. 1 .or. nbzc .ne. 1 .or. nbhy .ne. 1 .or. nbhz&
                                .ne. 1) then
                                call u2mess('E', 'MODELISA7_36')
                                else if (valepa(ihy).le.0.d0 .or. valepa(&
                            ihz).le.0.d0) then
                                call u2mess('E', 'MODELISA7_37')
                            else
                                call getvr8(nommcf, 'ANGL_VRIL', iocc, iarg, 0,&
                                            rbid, iangl)
                                if (iangl .eq. 0) then
                                    call u2mess('E', 'MODELISA7_38')
                                endif
                            endif
                        endif
                    endif
                endif
            endif
!
! --------3.6.DETECTION DE LA DERNIERE OCCURENCE POUR LAQUELLE LES
! --------    OPERANDES ASSOCIEES AUX CARACTERISTIQUES DES GRILLES
! --------    SONT PRESENTES
!
            call getvr8(nommcf, 'LONG_TYPG', iocc, iarg, 0,&
                        rbid, ntypg)
            if (ntypg .ne. 0) then
                ocgril = iocc
            endif
!
80      continue
!
! ------3.7.VERIFICATION DES COMPTEURS
!
        if (count1 .eq. 0) then
            call u2mess('E', 'MODELISA7_39')
        else if (count2.eq.0) then
            call u2mess('E', 'MODELISA7_40')
        else if (count3.eq.0) then
            call u2mess('E', 'MODELISA7_41')
        else if (count4.eq.0) then
            call u2mess('E', 'MODELISA7_42')
        else if (count5.eq.0) then
            call u2mess('E', 'MODELISA7_43')
        endif
!
! ------3.8.VERIFICATION DES DONNEES CARACTERISTIQUES DES GRILLES
!
        if (ocgril .ne. 0) then
            call tfvegr(nommcf, ocgril)
        endif
!
!
! ----4.CAS DE COQUE_COAX
!       -----------------
!
    else
!
        call getvr8(nommcf, 'VECT_X', 1, iarg, 0,&
                    rbid, ivect)
        if (abs(ivect) .ne. 3) then
            call u2mess('E', 'MODELISA7_44')
        else
            ier2 = 0
            call getvr8(nommcf, 'VECT_X', 1, iarg, 3,&
                        vect(1), ibid)
            if (vect(1) .eq. 1.d0) then
                if (vect(2) .ne. 0.d0 .or. vect(3) .ne. 0.d0) ier2 = 1
            else if (vect(2).eq.1.d0) then
                if (vect(1) .ne. 0.d0 .or. vect(3) .ne. 0.d0) ier2 = 1
            else if (vect(3).eq.1.d0) then
                if (vect(1) .ne. 0.d0 .or. vect(2) .ne. 0.d0) ier2 = 1
            else
                ier2 = 1
            endif
            if (ier2 .eq. 1) call u2mess('E', 'MODELISA7_45')
        endif
!
    endif
!
9999  continue
!
end subroutine
