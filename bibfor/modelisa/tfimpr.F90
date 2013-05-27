subroutine tfimpr(nom)
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
! IMPRESSION DANS LE FICHIER MESSAGE DES INFORMATIONS CONTENUES DANS
!                  UN CONCEPT DE TYPE TYPE_FLUI_STRU
!-----------------------------------------------------------------------
!  IN   : NOM   : NOM DU CONCEPT DE TYPE TYPE_FLUI_STRU
!-----------------------------------------------------------------------
!
    include 'jeveux.h'
    include 'asterfort/iunifi.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    character(len=3) :: nonoui(2)
    character(len=8) :: nomzo
    character(len=19) :: nom
    character(len=24) :: fsic, fsvi, fsvk, fsvr
!
    character(len=16) :: txence(2)
    character(len=24) :: texpas(2), txfaax(2), texaxe(3)
    character(len=60) :: textyp(4), txres1(8), txres2(4), txgra2(4)
    character(len=60) :: txres3(6), txres4(6)
!
!-----------------------------------------------------------------------
    integer :: iaxe, icoupl, ience, iequiv, ifm, imasse, ip
    integer :: ir, itypfl, izon, lfsic, lfsvi, lfsvk
    integer :: lfsvr, nzex
!-----------------------------------------------------------------------
    data nonoui  /'NON','OUI'/
!
    data textyp  /&
     &   'FAISCEAU DE TUBES SOUS ECOULEMENT TRANSVERSE               ',&
     &   'GRAPPE DE COMMANDE                                         ',&
     &   'FAISCEAU DE TUBES SOUS ECOULEMENT AXIAL                    ',&
     &   'DEUX COQUES CYLINDRIQUES COAXIALES - ECOULEMENT ANNULAIRE  '/
!
    data txres1  /&
     &   'CELLULE DE TUBES VIBRANTS EN DEBUT DE FAISCEAU VISCACHE1   ',&
     &   'CELLULE DE TUBES VIBRANTS CLOTAIRE PROFIL REEL             ',&
     &   'CELLULE DE TUBES VIBRANTS CLOTAIRE PROFIL UNIFORME         ',&
     &   'TUBE UNIQUE VIBRANT EN MILIEU DE FAISCEAU RIGIDE VISCACHE1 ',&
     &   'TUBE UNIQUE VIBRANT EN DEBUT DE FAISCEAU RIGIDE VISCACHE1  ',&
     &   'TUBE ROMPU                                                 ',&
     &   'TUBE UNIQUE VIBRANT EN MILIEU DE FAISCEAU RIGIDE TANAKA    ',&
     &   'TUBE UNIQUE VIBRANT EN MILIEU DE FAISCEAU RIGIDE DIVA EAU  '/
!
    data txres2  /&
     &   'CELLULE DE TUBES VIBRANTS EN DEBUT DE FAISCEAU VISCACHE1   ',&
     &   'CELLULE DE TUBES VIBRANTS EN MILIEU DE FAISCEAU VISCACHE1  ',&
     &   'TUBE UNIQUE VIBRANT EN MILIEU DE FAISCEAU RIGIDE VISCACHE1 ',&
     &   'TUBE UNIQUE VIBRANT EN DEBUT DE FAISCEAU RIGIDE VISCACHE1  '/
!
    data txres3  /&
     &   'TUBES VIBRANTS EN DEBUT DE FAISCEAU VISCACHE2 CFD 90 %     ',&
     &   'TUBES VIBRANTS EN DEBUT DE FAISCEAU VISCACHE2 CFD 85 %     ',&
     &   'TUBES VIBRANTS EN DEBUT DE FAISCEAU VISCACHE2 CFD 80 %     ',&
     &   'TUBES VIBRANTS EN DEBUT DE FAISCEAU VISCACHE2 CFD 50 %     ',&
     &   'TUBES VIBRANTS EN DEBUT DE FAISCEAU VISCACHE2 CFD 20 %     ',&
     &   'TUBES VIBRANTS EN DEBUT DE FAISCEAU VISCACHE2 CFD 10 %     '/
!
    data txres4  /&
     &   'TUBES VIBRANTS EN DEBUT DE FAISCEAU VISCACHE2 TUM 90 %     ',&
     &   'TUBES VIBRANTS EN DEBUT DE FAISCEAU VISCACHE2 TUM 86 %     ',&
     &   'TUBES VIBRANTS EN DEBUT DE FAISCEAU VISCACHE2 TUM 80 %     ',&
     &   'TUBES VIBRANTS EN DEBUT DE FAISCEAU VISCACHE2 TUM 50 %     ',&
     &   'TUBES VIBRANTS EN DEBUT DE FAISCEAU VISCACHE2 TUM 20 %     ',&
     &   'TUBES VIBRANTS EN DEBUT DE FAISCEAU VISCACHE2 TUM 10 %     '/
!
    data texpas  /'CARRE LIGNE             ',&
     &                'TRIANGULAIRE LIGNE      '/
!
    data txgra2  /&
     &   'GRAPPE2, ECOULEMENT ASCENDANT , TIGE DE COMMANDE CENTREE   ',&
     &   'GRAPPE2, ECOULEMENT ASCENDANT , TIGE DE COMMANDE EXCENTREE ',&
     &   'GRAPPE2, ECOULEMENT DESCENDANT, TIGE DE COMMANDE CENTREE   ',&
     &   'GRAPPE2, ECOULEMENT DESCENDANT, TIGE DE COMMANDE EXCENTREE '/
!
    data texaxe  /'AXE X DU REPERE GLOBAL  ',&
     &                'AXE Y DU REPERE GLOBAL  ',&
     &                'AXE Z DU REPERE GLOBAL  '/
!
    data txfaax  /'FAISCEAU COMPLET        ',&
     &                'FAISCEAU EQUIVALENT     '/
!
    data txence  /'CIRCULAIRE      ',&
     &                'RECTANGULAIRE   '/
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
    90   format(3x,'***************************************************',&
     &         '****')
    91   format(3x,'*',53x,'*')
    92   format(3x,'*',1x,'DEFINITION DES CARACTERISTIQUES D UNE ',&
     &         'CONFIGURATION',1x,'*')
    93   format(3x,'*',6x,'POUR UNE ETUDE DYNAMIQUE SOUS ECOULEMENT',&
     &         7x,'*')
    94   format(3x,'CONFIGURATION : ',a60)
    95   format(3x,'PRISE EN COMPTE DU COUPLAGE : ',a3)
!
    101   format(5x, a60)
    102   format(3x,'TYPE DE PAS DU FAISCEAU : ',a24)
    103   format(3x,'VALEUR DU PAS REDUIT : ',d12.5)
    104   format(3x,'VALEUR DU CONFINEMENT : ',d12.5)
    105   format(3x,'TYPE DE RESEAU DE LA ZONE ASSOCIEE AU PROFIL '&
     &        ,a8,':')
!
    201   format(3x,'EXCITATION : ',a60)
    202   format(3x,'NOEUD D APPLICATION : ',a8)
    203   format(3x,'VALEUR DU COEFFICIENT DE MASSE AJOUTEE : ',d12.5)
!
    301   format(3x,'TYPE DE REPRESENTATION : ',a24)
    302   format(3x,'AXE DIRECTEUR DU FAISCEAU : ',a24)
    311   format(3x,'NOMBRE DE TUBES DU FAISCEAU EQUIVALENT : ',i3)
    312   format(3x,'NOMBRE DE TUBES DU FAISCEAU REEL : ',i3)
    321   format(3x,'TYPE D ENCEINTE : ',a16)
!
    401   format(3x,'PRISE EN COMPTE DES EFFETS DE MASSE AJOUTEE : ',a3)
    402   format(3x,'AXE DE REVOLUTION DES COQUES : ',a24)
!
    ifm = iunifi('MESSAGE')
!
    fsic = nom//'.FSIC'
    fsvi = nom//'.FSVI'
    fsvk = nom//'.FSVK'
    fsvr = nom//'.FSVR'
!
    call jeveuo(fsic, 'L', lfsic)
    itypfl = zi(lfsic)
    icoupl = zi(lfsic+1)
!
    write(ifm,*)
    write(ifm,90)
    write(ifm,91)
    write(ifm,92)
    write(ifm,91)
    write(ifm,93)
    write(ifm,91)
    write(ifm,90)
    write(ifm,*)
    write(ifm,94) textyp(itypfl)
    write(ifm,*)
    write(ifm,95) nonoui(icoupl+1)
    write(ifm,*)
!
    if (itypfl .eq. 1) then
!
        call jeveuo(fsvr, 'L', lfsvr)
        if (icoupl .eq. 0) then
            write(ifm,104) zr(lfsvr)
        else
            call jeveuo(fsvi, 'L', lfsvi)
            call jeveuo(fsvk, 'L', lfsvk)
            ip = zi(lfsvi)
            nzex = zi(lfsvi+1)
            if (ip .eq. 1) then
                do 10 izon = 1, nzex
                    ir = zi(lfsvi+izon+1)
                    nomzo = zk8(lfsvk+izon+3)
                    write(ifm,105) nomzo
                    if (ir .lt. 100) then
                        write(ifm,101) txres1(ir)
                    else if (ir.lt.200) then
                        write(ifm,101) txres3(ir-100)
                    else
                        write(ifm,101) txres4(ir-1000)
                    endif
10              continue
            else
                do 20 izon = 1, nzex
                    ir = zi(lfsvi+izon+1)
                    nomzo = zk8(lfsvk+izon+3)
                    write(ifm,105) nomzo
                    write(ifm,101) txres2(ir)
20              continue
            endif
            write(ifm,102) texpas(ip)
            write(ifm,103) zr(lfsvr+1)
            write(ifm,104) zr(lfsvr)
        endif
!
    else if (itypfl.eq.2) then
!
        if (icoupl .eq. 1) then
            call jeveuo(fsvk, 'L', lfsvk)
            call jeveuo(fsvr, 'L', lfsvr)
            if (zk8(lfsvk) .eq. 'ASC_CEN') then
                write(ifm,201) txgra2(1)
            else if (zk8(lfsvk).eq.'ASC_EXC') then
                write(ifm,201) txgra2(2)
            else if (zk8(lfsvk).eq.'DES_CEN') then
                write(ifm,201) txgra2(3)
            else
                write(ifm,201) txgra2(4)
            endif
            write(ifm,202) zk8(lfsvk+1)
            write(ifm,203) zr(lfsvr)
        endif
!
    else if (itypfl.eq.3) then
!
        call jeveuo(fsvi, 'L', lfsvi)
        iequiv = zi(lfsvi)
        iaxe = zi(lfsvi+1)
        ience = zi(lfsvi+2)
        write(ifm,301) txfaax(iequiv+1)
        write(ifm,302) texaxe(iaxe)
        if (iequiv .eq. 1) then
            write(ifm,311) zi(lfsvi+3)
            write(ifm,312) zi(lfsvi+4)
        endif
        write(ifm,321) txence(ience)
!
    else
!
        call jeveuo(fsvi, 'L', lfsvi)
        imasse = zi(lfsvi)
        iaxe = zi(lfsvi+1)
        write(ifm,401) nonoui(imasse+1)
        write(ifm,402) texaxe(iaxe)
!
    endif
!
    write(ifm,*)
!
    call jedema()
end subroutine
