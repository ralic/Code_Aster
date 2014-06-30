subroutine cfmxsd(noma, nomo, numedd, fonact, sddyna,&
                  defico, resoco, ligrcf, ligrxf)
!
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: mickael.abbas at edf.fr
!
    implicit none
#include "jeveux.h"
#include "asterfort/cfcrsd.h"
#include "asterfort/cfdisi.h"
#include "asterfort/cfdisl.h"
#include "asterfort/cfmmap.h"
#include "asterfort/cfmmci.h"
#include "asterfort/cfmmma.h"
#include "asterfort/cfmmvd.h"
#include "asterfort/cfmxme.h"
#include "asterfort/cfmxr0.h"
#include "asterfort/infdbg.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/wkvect.h"
#include "asterfort/xxmxme.h"
    character(len=8) :: noma, nomo
    character(len=24) :: numedd
    integer :: fonact(*)
    character(len=19) :: sddyna
    character(len=24) :: defico, resoco
    character(len=19) :: ligrcf, ligrxf
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (TOUTES METHODES)
!
! CREATION DES SDS DE RESOLUTION DU CONTACT (RESOCO)
!
! ----------------------------------------------------------------------
!
!
! IN  NOMA   : NOM DU MAILLAGE
! IN  NOMO   : NOM DU MODELE
! IN  NUMEDD : NUME_DDL DE LA MATRICE TANGENTE GLOBALE
! IN  FONACT : FONCTIONNALITES ACTIVEES (VOIR NMFONC)
! IN  SDDYNA : SD DYNAMIQUE
! IN  DEFICO : SD POUR LA DEFINITION DE CONTACT
! IN  RESOCO : SD POUR LA RESOLUTION DE CONTACT
! IN  LIGRCF : NOM DU LIGREL TARDIF POUR ELEMENTS DE CONTACT CONTINUE
! IN  LIGRXF : NOM DU LIGREL TARDIF POUR ELEMENTS DE CONTACT XFEM GG
!
!
!
!
    integer :: zbouc, ztaco
    integer :: nzoco
    integer :: ifm, niv
    logical(kind=1) :: lctcd, lctcc, lxfcm, lmail, lallv
    character(len=24) :: mboucl, tabcof
    integer :: jmbouc, jtabco
    character(len=24) :: nosdco
    integer :: jnosdc
    character(len=14) :: numedf
    character(len=24) :: crnudd, maxdep
    integer :: jcrnud, jmaxde
!
! ----------------------------------------------------------------------
!
    call jemarq()
    call infdbg('CONTACT', ifm, niv)
!
! --- FONCTIONNALITES ACTIVEES
!
    lmail = cfdisl(defico,'FORMUL_MAILLEE')
    lxfcm = cfdisl(defico,'FORMUL_XFEM')
    lctcc = cfdisl(defico,'FORMUL_CONTINUE')
    lctcd = cfdisl(defico,'FORMUL_DISCRETE')
    lallv = cfdisl(defico,'ALL_VERIF')
!
! --- INITIALISATIONS
!
    zbouc = cfmmvd('ZBOUC')
    ztaco = cfmmvd('ZTACO')
! --- NUME_DDL MATRICE FROTTEMENT
    numedf = '&&CFMXSD.NUMDF'
    nzoco = cfdisi(defico,'NZOCO')
!
! --- CREATION DES SD RESULTATS: VALE_CONT ET PERCUSSIONS
!
    call cfmxr0(defico, resoco, noma)
!
! --- CREATION DE LA SD APPARIEMENT
!
    if (lmail) then
        call cfmmap(noma, defico, resoco)
    endif
!
! --- CREATION DES COMPTEURS DE BOUCLE
!
    mboucl = resoco(1:14)//'.MBOUCL'
    call wkvect(mboucl, 'V V I', zbouc, jmbouc)
!
! --- STOCKAGE NOM DES SD DE DONNEES TYPE LIGREL ET CARTE
!
    nosdco = resoco(1:14)//'.NOSDCO'
    call wkvect(nosdco, 'V V K24', 5, jnosdc)
    zk24(jnosdc+1-1) = numedf
    zk24(jnosdc+2-1) = ligrcf
    zk24(jnosdc+3-1) = ligrxf
!
! --- TABLEAU DES COEFFICIENTS
!
    tabcof = resoco(1:14)//'.TABL.COEF'
    call wkvect(tabcof, 'V V R', nzoco*ztaco, jtabco)
!
! --- INITIALISE LES COEFFICIENTS VARIABLES
!
    call cfmmci(defico, resoco)
!
! --- LOGICAL POUR DECIDER DE REFAIRE LE NUME_DDL OU NON
!
    if (lctcc) then
        crnudd = resoco(1:14)//'.NUDD'
        call wkvect(crnudd, 'V V L', 1, jcrnud)
        if (lallv) then
            zl(jcrnud) = .false.
        else
            zl(jcrnud) = .true.
        endif
    endif
!
! --- PARAMETRE POUR LA REACTUALISATION AUTOMATIQUE
!
    maxdep = resoco(1:14)//'.MAXD'
    call wkvect(maxdep, 'V V R', 1, jmaxde)
    zr(jmaxde) = -1.d0
!
! --- MODE ALL VERIF
!
    if (lallv) then
        goto 99
    endif
!
! --- AFFICHAGE
!
    if (niv .ge. 2) then
        write (ifm,*) '<CONTACT> CREATION SD DE RESOLUTION'
    endif
!
! --- CREATION DES SD POUR METHODES MAILLEES
!
    if (lmail) then
        call cfmmma(defico, resoco)
    endif
!
! --- CONTACT DISCRET
!
    if (lctcd) then
        call cfcrsd(noma, numedd, defico, resoco)
    endif
!
! --- CONTACT CONTINU
!
    if (lctcc) then
        call cfmxme(numedd, sddyna, defico, resoco)
    endif
!
! --- CONTACT XFEM
!
    if (lxfcm) then
        call xxmxme(noma, nomo, fonact, defico, resoco)
    endif
!
99  continue
!
    call jedema()
!
end subroutine
