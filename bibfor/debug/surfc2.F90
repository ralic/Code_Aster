subroutine surfc2(char, noma, ifm)
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
#include "asterfort/cfdisi.h"
#include "asterfort/cfmmvd.h"
#include "asterfort/cfnumm.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/mminfi.h"
#include "asterfort/mminfl.h"
#include "asterfort/mminfm.h"
#include "asterfort/mminfr.h"
#include "asterfort/wkvect.h"
    character(len=8) :: char
    character(len=8) :: noma
    integer :: ifm
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODE CONTINUE - AFFICHAGE DONNEES)
!
! AFFICHAGE LES INFOS CONTENUES DANS LA SD CONTACT POUR LA FORMULATION
! CONTINUE
!
! ----------------------------------------------------------------------
!
!
! IN  CHAR   : NOM UTILISATEUR DU CONCEPT DE CHARGE
! IN  NOMA   : NOM DU MAILLAGE
! IN  IFM    : UNITE D'IMPRESSION
!
! ----------------------------------------------------------------------
!
    integer :: nzoco, nnoco, nmaco
    real(kind=8) :: tolint
    logical :: lveri
    integer :: izone
    character(len=24) :: mailma, defico
    integer :: jnomno, jnomma
    character(len=8) :: nommae
    integer :: nummae, posmae, imae, jdecme
    integer :: nbmae
    integer :: ndexfr, nptm
!
    character(len=24) :: paracr, paraci
    integer :: jparcr, jparci
    character(len=24) :: caracf
    integer :: jcmcf
    character(len=24) :: contno, contma
    integer :: jnoco, jmaco
    integer :: zcmcf
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATIONS
!
    defico = char(1:8)//'.CONTACT'
!
! --- ACCES SD DU MAILLAGE
!
    mailma = noma(1:8)//'.NOMMAI'
!
! --- COMMUNS TOUTES FORMULATIONS
!
    paracr = defico(1:16)//'.PARACR'
    paraci = defico(1:16)//'.PARACI'
    call jeveuo(paracr, 'L', jparcr)
    call jeveuo(paraci, 'L', jparci)
!
! --- COMMUNS AVEC FORM. MAILLEES (DISCRET ET CONTINUE MAIS PAS XFEM)
!
    contno = defico(1:16)//'.NOEUCO'
    contma = defico(1:16)//'.MAILCO'
    call jeveuo(contno, 'L', jnoco)
    call jeveuo(contma, 'L', jmaco)
!
! --- SPECIFIQUES FORMULATION CONTINUE
!
    caracf = defico(1:16)//'.CARACF'
!
    call jeveuo(caracf, 'L', jcmcf)
!
    zcmcf = cfmmvd('ZCMCF')
!
! --- INITIALISATIONS
!
    nzoco = cfdisi(defico,'NZOCO')
    nmaco = cfdisi(defico,'NMACO')
    nnoco = cfdisi(defico,'NNOCO')
!
! --- CREATION VECTEURS TEMPORAIRES
!
    call wkvect('&&SURFC2.TRAVNO', 'V V K8', nzoco*nnoco, jnomno)
    call wkvect('&&SURFC2.TRAVMA', 'V V K8', nzoco*nmaco, jnomma)
!
! --- IMPRESSIONS POUR L'UTILISATEUR
!
    write (ifm,*)
    write (ifm,*) '<CONTACT> INFOS SPECIFIQUES SUR LA FORMULATION'//&
     &              ' CONTINUE'
    write (ifm,*)
!
! --- IMPRESSIONS POUR LES PARAMETRES CONSTANTS
!
    write (ifm,*) '<CONTACT> ... PARAMETRES CONSTANTS SUR TOUTES '//&
     &              ' LES ZONES'
!
    write (ifm,1070) 'AXISYMETRIE     ',zi(jparci+16-1)
!
    1070 format (' <CONTACT> ...... PARAM. : ',a16,' - VAL. : ',i5)
    1071 format (' <CONTACT> ...... PARAM. : ',a16,' - VAL. : ',e12.5)
!
! --- IMPRESSIONS POUR LES PARAMETRES VARIABLES
!
    write (ifm,*) '<CONTACT> ... PARAMETRES VARIABLES SUIVANT '//&
     &              ' LES ZONES'
    do 320 izone = 1, nzoco
        write (ifm,*) '<CONTACT> ...... ZONE : ',izone
        lveri = mminfl(defico,'VERIF',izone )
        if (lveri) then
            write (ifm,*) '<CONTACT> ...... ZONE DE VERIFICATION'
            tolint = mminfr(defico,'TOLE_INTERP',izone)
            write (ifm,1071) 'TOLE_INTERP     ',tolint
        else
            write (ifm,*) '<CONTACT> ...... ZONE DE CALCUL'
            write (ifm,1071) 'INTEGRATION     ',&
     &                      zr(jcmcf+zcmcf*(izone-1)-1+1)
            write (ifm,1071) 'COEF_CONT      ',&
     &                      zr(jcmcf+zcmcf*(izone-1)-1+2)
            write (ifm,1071) 'ALGO_CONT       ',&
     &                      zr(jcmcf+zcmcf*(izone-1)-1+3)
            write (ifm,1071) 'COEF_FROT       ',&
     &                      zr(jcmcf+zcmcf*(izone-1)-1+4)
            write (ifm,1071) 'ALGO_FROT       ',&
     &                      zr(jcmcf+zcmcf*(izone-1)-1+5)
            write (ifm,1071) 'COULOMB         ',&
     &                      zr(jcmcf+zcmcf*(izone-1)-1+6)
            write (ifm,1071) 'SEUIL_INIT      ',&
     &                      zr(jcmcf+zcmcf*(izone-1)-1+7)
            write (ifm,1071) 'CONTACT_INIT    ',&
     &                      zr(jcmcf+zcmcf*(izone-1)-1+8)
            write (ifm,1071) 'GLISSIERE       ',&
     &                      zr(jcmcf+zcmcf*(izone-1)-1+9)
            write (ifm,1071) 'SANS_NOEUD      ',&
     &                      zr(jcmcf+zcmcf*(izone-1)-1+10)
            write (ifm,1071) 'SANS_NOEUD_FR   ',&
     &                      zr(jcmcf+zcmcf*(izone-1)-1+11)
            write (ifm,1071) 'EXCL_FROT_X     ',&
     &                      zr(jcmcf+zcmcf*(izone-1)-1+12)
        endif
!
320  end do
!
! ---  MAILLES ESCLAVES
!
    write (ifm,*) '<CONTACT> ... INFORMATIONS SUR MAILLES ESCLAVES'
!
    do 900 izone = 1, nzoco
        write (ifm,*) '<CONTACT> ...... ZONE : ',izone
        jdecme = mminfi(defico,'JDECME',izone )
        nbmae = mminfi(defico,'NBMAE' ,izone )
        do 901 imae = 1, nbmae
            posmae = jdecme + imae
!
            call mminfm(posmae, defico, 'NPTM', nptm)
            call cfnumm(defico, posmae, nummae)
            call jenuno(jexnum(mailma, nummae), nommae)
            write (ifm,1080) nommae
!
            write (ifm,1070) 'ZONE            ',izone
            write (ifm,1070) 'NB. PTS. INT.   ',nptm
!
            call mminfm(posmae, defico, 'NDEXFR', ndexfr)
!
            write (ifm,1070) 'NDEXFR          ',ndexfr
!
!
901      continue
900  end do
    1080 format (' <CONTACT> ... MAILLE : ',a8)
!
! --- MENAGE
!
    call jedetr('&&SURFC2.TRAVNO')
    call jedetr('&&SURFC2.TRAVMA')
!
    call jedema()
end subroutine
