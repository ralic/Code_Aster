subroutine op9999()
    implicit none
! TOLE  CRS_513
!     ------------------------------------------------------------------
! person_in_charge: j-pierre.lefebvre at edf.fr
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
!     OPERATEUR DE CLOTURE
!     ------------------------------------------------------------------
!     FIN OP9999
!     ------------------------------------------------------------------
    include 'jeveux.h'
    include 'asterc/gettyp.h'
    include 'asterc/getvis.h'
    include 'asterc/getvtx.h'
    include 'asterc/jdcset.h'
    include 'asterfort/fin999.h'
    include 'asterfort/iunifi.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jedetc.h'
    include 'asterfort/jefini.h'
    include 'asterfort/jeimhd.h'
    include 'asterfort/jeliad.h'
    include 'asterfort/jelibf.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jetass.h'
    include 'asterfort/jxcopy.h'
    include 'asterfort/jxveri.h'
    include 'asterfort/rsinfo.h'
    include 'asterfort/u2mess.h'
    include 'asterfort/uimpba.h'
    include 'asterfort/ulexis.h'
    include 'asterfort/ulopen.h'
    include 'asterfort/wkvect.h'
    integer :: info, nbenre, nboct
    integer :: ifm, iunerr, iunres, iunmes
    integer :: i, l, jco, nbco
    integer :: nbext, nfhdf, iarg
    character(len=8) :: k8b, ouinon
    character(len=16) :: fchier, fhdf, typres
    character(len=80) :: fich
!     ------------------------------------------------------------------
!
    call jemarq()
    info = 1
!
! --- FERMETURE DES BIBLIOTHEQUES / VERIFICATION DES ALARMES ET ERREURS
!
    call fin999()
!
!
!
    ifm = 0
    fchier = ' '
    call getvis(' ', 'UNITE', 1, iarg, 1,&
                ifm, l)
    if (.not. ulexis( ifm )) then
        call ulopen(ifm, ' ', fchier, 'NEW', 'O')
    endif
!
    typres = 'RESULTAT_SDASTER'
    nbco = 0
    call gettyp(typres, nbco, k8b)
    if (nbco .gt. 0) then
        call wkvect('&&OP9999.NOM', 'V V K8', nbco, jco)
        call gettyp(typres, nbco, zk8(jco))
        do 10 i = 1, nbco
            write(ifm,1000)
            call rsinfo(zk8(jco-1+i), ifm)
10      continue
    endif
!
    iunerr = iunifi('ERREUR')
    iunmes = iunifi('MESSAGE')
    iunres = iunifi('RESULTAT')
!
!     --- SUPPRESSION DES CONCEPTS TEMPORAIRES DES MACRO
    call jedetc('G', '.', 1)
!
!     -- IMPRESSION DE LA TAILLE DES CONCEPTS DE LA BASE GLOBALE:
    call uimpba('G', iunmes)
!
!     --- RETASSAGE EVENTUEL DE LA GLOBALE
    call getvtx(' ', 'RETASSAGE', 1, iarg, 1,&
                ouinon, l)
    if (ouinon .eq. 'OUI') call jetass('G')
!
!     --- SAUVEGARDE DE LA GLOBALE AU FORMAT HDF
    fhdf = 'NON'
    call getvtx(' ', 'FORMAT_HDF', 1, iarg, 1,&
                fhdf, nfhdf)
    if (nfhdf .gt. 0) then
        if (fhdf .eq. 'OUI') then
            if (ouinon .eq. 'OUI') then
                call u2mess('A', 'SUPERVIS2_8')
            endif
            fich = 'bhdf.1'
            call jeimhd(fich, 'G')
        endif
    endif
!
!     RECUPERE LA POSITION D'UN ENREGISTREMENT SYSTEME CARACTERISTIQUE
    call jeliad('G', nbenre, nboct)
    call jdcset('jeveux_sysaddr', nboct)
!
!     --- APPEL JXVERI POUR VERIFIER LA BONNE FIN D'EXECUTION
    call jxveri()
!
!     --- CLOTURE DES FICHIERS ---
    call jelibf('SAUVE', 'G', info)
    if (iunerr .gt. 0) write(iunerr,* ) '<I> <FIN> FERMETURE DE LA BASE "GLOBALE" EFFECTUEE.'
    if (iunres .gt. 0) write(iunres,* ) '<I> <FIN> FERMETURE DE LA BASE "GLOBALE" EFFECTUEE.'
!
    call jelibf('DETRUIT', 'V', info)
!
!     --- RETASSAGE EFFECTIF ----
    if (ouinon .eq. 'OUI') then
        call jxcopy('G', 'GLOBALE', 'V', 'VOLATILE', nbext)
        if (iunerr .gt. 0) write(iunerr, '(A,I2,A)'&
                           ) ' <I> <FIN> RETASSAGE DE LA BASE "GLOBALE" EFFECTUEE, ',&
                           nbext, ' FICHIER(S) UTILISE(S).'
        if (iunres .gt. 0) write(iunres, '(A,I2,A)'&
                           ) ' <I> <FIN> RETASSAGE DE LA BASE "GLOBALE" EFFECTUEE, ',&
                           nbext, ' FICHIER(S) UTILISE(S).'
    endif
!
!     --- IMPRESSION DES STATISTIQUES ( AVANT CLOTURE DE JEVEUX ) ---
    call u2mess('I', 'SUPERVIS2_97')
    if (iunerr .gt. 0) write(iunerr, *) '<I> <FIN> ARRET NORMAL DANS "FIN" PAR APPEL A "JEFINI".'
    if (iunres .gt. 0) write(iunres, *) '<I> <FIN> ARRET NORMAL DANS "FIN" PAR APPEL A "JEFINI".'
!
!     --- LA CLOTURE DE JEVEUX ---
!
    call jefini('NORMAL')
!
    1000 format(/,1x,'======>')
!
    call jedema()
end subroutine
