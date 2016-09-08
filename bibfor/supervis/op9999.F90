subroutine op9999()
    use parameters_module
    implicit none
!     ------------------------------------------------------------------
! person_in_charge: j-pierre.lefebvre at edf.fr
! ======================================================================
! COPYRIGHT (C) 1991 - 2016  EDF R&D                  WWW.CODE-ASTER.ORG
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
!-----------------------------------------------------------------------
!     FIN OP9999
!-----------------------------------------------------------------------
#include "jeveux.h"
#include "asterc/gettyp.h"
#include "asterc/jdcset.h"
#include "asterfort/assert.h"
#include "asterfort/fin999.h"
#include "asterfort/getvis.h"
#include "asterfort/getvtx.h"
#include "asterfort/iunifi.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetc.h"
#include "asterfort/jefini.h"
#include "asterfort/jeimhd.h"
#include "asterfort/jeliad.h"
#include "asterfort/jelibf.h"
#include "asterfort/jemarq.h"
#include "asterfort/jetass.h"
#include "asterfort/jxcopy.h"
#include "asterfort/jxveri.h"
#include "asterfort/rsinfo.h"
#include "asterfort/ststat.h"
#include "asterfort/uimpba.h"
#include "asterfort/ulexis.h"
#include "asterfort/ulopen.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
#include "asterfort/asmpi_info.h"
    mpi_int :: mrank, msize
    integer :: info, nbenre, nboct, iret, nbrank
    integer :: ifm, iunerr, iunres, iunmes
    integer :: i, jco, nbco
    integer :: nbext, nfhdf, nproc
    aster_logical :: bool
    character(len=8) :: k8b, ouinon, infr, proc
    character(len=16) :: fchier, fhdf, typres
    character(len=80) :: fich
!-----------------------------------------------------------------------
!
    call jemarq()
    info = 1

    call asmpi_info(rank=mrank, size=msize)
    nbrank = to_aster_int(mrank)
!       
!   --- PROC0 = 'OUI' pour effectuer les ecritures uniquement sur le processeur de rang 0 ---
!       si PROC0 = 'NON' on force nbrank=0 
    call getvtx(' ', 'PROC0', scal=proc, nbret=nproc)
    if ( proc .eq. 'NON' ) then 
      nbrank = 0 
    endif   
    call getvis(' ', 'STATUT', scal=iret)
    bool = iret == ST_ER .or. iret == ST_OK .or. iret == ST_ER_PR0 .or. &
           iret == ST_ER_OTH .or. iret == ST_UN_OTH .or. iret == ST_EXCEPT
    ASSERT(bool)
    call ststat(iret)

! --- MENAGE DANS LES BIBLIOTHEQUES, ALARMES, ERREURS, MPI
!
    call fin999()
!
! --- ecriture des informations sur le contenu de chaque sd_resultat :
!
    call getvtx(' ', 'INFO_RESU', scal=infr)
    if (infr.eq.'OUI') then
        ifm = 0
        fchier = ' '
        call getvis(' ', 'UNITE', scal=ifm)
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
                write(ifm,100)
                call rsinfo(zk8(jco-1+i), ifm)
10          continue
        endif
    endif
!
    iunerr = iunifi('ERREUR')
    iunmes = iunifi('MESSAGE')
    iunres = iunifi('RESULTAT')
!
! --- SUPPRESSION DES CONCEPTS TEMPORAIRES DES MACRO
!
    if ( nbrank .eq. 0 ) then
      call jedetc('G', '.', 1)
!
! --- IMPRESSION DE LA TAILLE DES CONCEPTS DE LA BASE GLOBALE
!
      call uimpba('G', iunmes)
!
! --- RETASSAGE EVENTUEL DE LA GLOBALE
!
      call getvtx(' ', 'RETASSAGE', scal=ouinon)
      if (ouinon .eq. 'OUI') call jetass('G')
!
! --- SAUVEGARDE DE LA GLOBALE AU FORMAT HDF
!
      fhdf = 'NON'
      call getvtx(' ', 'FORMAT_HDF', scal=fhdf, nbret=nfhdf)
      if (nfhdf .gt. 0) then
        if (fhdf .eq. 'OUI') then
            if (ouinon .eq. 'OUI') then
                call utmess('A', 'SUPERVIS2_8')
            endif
            fich = 'bhdf.1'
            call jeimhd(fich, 'G')
        endif
     endif
   endif
     
!
! --- RECUPERE LA POSITION D'UN ENREGISTREMENT SYSTEME CARACTERISTIQUE
!
    call jeliad('G', nbenre, nboct)
    call jdcset('jeveux_sysaddr', nboct)
!
! --- APPEL JXVERI POUR VERIFIER LA BONNE FIN D'EXECUTION
!
    if ( nbrank .eq. 0 ) then
      call jxveri()
!
! --- CLOTURE DES FICHIERS
!
      call jelibf('SAUVE', 'G', info)
      if (iunerr .gt. 0) write(iunerr,* ) '<I> <FIN> FERMETURE DE LA BASE "GLOBALE" EFFECTUEE.'
      if (iunres .gt. 0) write(iunres,* ) '<I> <FIN> FERMETURE DE LA BASE "GLOBALE" EFFECTUEE.'
!
      call jelibf('DETRUIT', 'V', info)
!
! --- RETASSAGE EFFECTIF
!
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
! --- IMPRESSION DES STATISTIQUES ( AVANT CLOTURE DE JEVEUX )
!
      call utmess('I', 'SUPERVIS2_97')
      if (iunerr .gt. 0) write(iunerr, *) '<I> <FIN> ARRET NORMAL DANS "FIN" PAR APPEL A "JEFINI".'
      if (iunres .gt. 0) write(iunres, *) '<I> <FIN> ARRET NORMAL DANS "FIN" PAR APPEL A "JEFINI".'
    endif  
    call jedema()
!
! --- CLOTURE DE JEVEUX
!
    call jefini('NORMAL' , arg_rank=nbrank)
!
!-----------------------------------------------------------------------
!
    100 format(/,1x,'======>')
!
end subroutine
