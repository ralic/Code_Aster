subroutine surfc3(char, noma, ifm)
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: mickael.abbas at edf.fr
!
    implicit none
#include "asterf_types.h"
#include "jeveux.h"
!
#include "asterfort/cfdisi.h"
#include "asterfort/cfdisl.h"
#include "asterfort/cfmmvd.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
    character(len=8) :: noma, char
    integer :: ifm
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODE XFEM   - AFFICHAGE DONNEES)
!
! AFFICHAGE LES INFOS CONTENUES DANS LA SD CONTACT POUR LA FORMULATION
! XFEM
!
! ----------------------------------------------------------------------
!
!
! IN  NOMA   : NOM DU MAILLAGE
! IN  CHAR   : NOM UTILISATEUR DU CONCEPT DE CHARGE
! IN  IFM    : UNITE D'IMPRESSION
!
!
!
!
    integer :: zcmxf, zmesx
    aster_logical :: ltfcm
    integer :: nzoco
    integer :: izone, statut
    character(len=24) :: mailma, defico
    character(len=8) :: nommae, nomzon
    integer :: imae, ntmae, nummae
!
    character(len=24) :: caraxf, maescx
    integer :: jcmxf, jmaesx
!
!      CHARACTER*24 XFIESC,XSIESC,XSIMAI
!      INTEGER      JFIESC,JSIESC,JSIMAI
    character(len=24) :: xfimai
    integer :: jfimai
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATIONS
!
    defico = char(1:8)//'.CONTACT'
    nzoco = cfdisi(defico,'NZOCO')
    ltfcm = cfdisl(defico,'CONT_XFEM_GG')
    ntmae = cfdisi(defico,'NTMAE')
!
! --- ACCES SD DU MAILLAGE
!
    mailma = noma(1:8)//'.NOMMAI'
!
! --- COMMUNS AVEC FORM. CONTINUE
!
    caraxf = defico(1:16)//'.CARAXF'
    call jeveuo(caraxf, 'L', jcmxf)
!
    zcmxf = cfmmvd('ZCMXF')
!
! --- SPECIFIQUES XFEM
!
!     XFIESC = DEFICO(1:16)//'.XFIESC'
!     XSIESC = DEFICO(1:16)//'.XSIESC'
!     XSIMAI = DEFICO(1:16)//'.XSIMAI'
    xfimai = defico(1:16)//'.XFIMAI'
!
    call jeveuo(xfimai, 'L', jfimai)
!     CALL JEVEUO(XFIESC,'L',JFIESC)
!     CALL JEVEUO(XSIESC,'L',JSIESC)
!     CALL JEVEUO(XSIMAI,'L',JSIMAI)
!
!
! --- IMPRESSIONS POUR L'UTILISATEUR
!
    write (ifm,*)
    write (ifm,*) '<CONTACT> INFOS SPECIFIQUES SUR LA FORMULATION'//&
     &              ' XFEM'
    write (ifm,*)
!
    write (ifm,*) '<CONTACT> ... ZONES XFEM.'
    do 610 izone = 1, nzoco
        write (ifm,*) '<CONTACT> ...... ZONE : ',izone
        write (ifm,1010) zk8(jfimai-1+izone)
!        WRITE (IFM,1011) ZI(JSIMAI-1+IZONE)
!       WRITE (IFM,1090) ZK8(JFIESC-1+IZONE)
!       WRITE (IFM,1091) ZI(JSIESC-1+IZONE)
610 end do
!
    1010 format (' <CONTACT> ...... FISS. MAITRE : ',a18)
!
! --- IMPRESSIONS POUR LES PARAMETRES VARIABLES
!
    write (ifm,*) '<CONTACT> ... PARAMETRES VARIABLES SUIVANT '//&
     &              ' LES ZONES'
    do 320 izone = 1, nzoco
        write (ifm,*) '<CONTACT> ...... ZONE : ',izone
!
        write (ifm,1071) 'INTEGRATION     ',zr(jcmxf+zcmxf*(izone-1)+1-1)
        write (ifm,1071) 'COEF_REGU_CONT  ',zr(jcmxf+zcmxf*(izone-1)+2-1)
        write (ifm,1071) 'COEF_REGU_FROT  ',zr(jcmxf+zcmxf*(izone-1)+3-1)
        write (ifm,1071) 'COEF_STAB_CONT  ',zr(jcmxf+zcmxf*(izone-1)+11-1)
        write (ifm,1071) 'COEF_PENA_CONT  ',zr(jcmxf+zcmxf*(izone-1)+12-1)
        write (ifm,1071) 'COEF_STAB_FROT  ',zr(jcmxf+zcmxf*(izone-1)+13-1)
        write (ifm,1071) 'COEF_PENA_CONT  ',zr(jcmxf+zcmxf*(izone-1)+14-1)
        write (ifm,1071) 'FROTTEMENT      ',zr(jcmxf+zcmxf*(izone-1)+5-1)
        write (ifm,1071) 'COULOMB         ',zr(jcmxf+zcmxf*(izone-1)+4-1)
        write (ifm,1071) 'SEUIL_INIT      ',zr(jcmxf+zcmxf*(izone-1)+6-1)
        write (ifm,1071) 'CONTACT_INIT    ',zr(jcmxf+zcmxf*(izone-1)+7-1)
        write (ifm,1071) 'COEF_ECHELLE    ',zr(jcmxf+zcmxf*(izone-1)+8-1)
        write (ifm,1071) 'ALGO_LAGR       ',zr(jcmxf+zcmxf*(izone-1)+9-1)
        write (ifm,1071) 'GLISSIERE       ',zr(jcmxf+zcmxf*(izone-1)+10-1)
320 end do
!
! ---  MAILLES ESCLAVES SPECIFIQUES
!
    if (ltfcm) then
        maescx = defico(1:16)//'.MAESCX'
        zmesx = cfmmvd('ZMESX')
        call jeveuo(maescx, 'L', jmaesx)
        write (ifm,*) '<CONTACT> ... INFORMATIONS SUR MAILLES ESCLAVES'
        do 900 izone = 1, nzoco
            nomzon = zk8(jfimai-1+izone)
            write (ifm,*) '<CONTACT> ...... ZONE : ',izone
            write (ifm,1010) nomzon
            do 901 imae = 1, ntmae
!
                nummae = zi(jmaesx+zmesx*(imae-1)+1-1)
                call jenuno(jexnum(mailma, nummae), nommae)
                write (ifm,1080) nommae
!
!
                write (ifm,1070) 'ZONE            ',&
     &           zi(jmaesx+zmesx*(imae-1)+2-1)
                write (ifm,1070) 'NB. PTS. INT.   ',&
     &           zi(jmaesx+zmesx*(imae-1)+3-1)
!
                statut = zi(jmaesx+zmesx*(imae-1)+1-1)
!
                if (statut .eq. 0) then
                    write (ifm,1040) 'PAS DE FOND. FISS.'
                else if (statut.eq.1) then
                    write (ifm,1040) 'HEAVISIDE'
                else if (statut.eq.-2) then
                    write (ifm,1040) 'CRACK-TIP'
                else if (statut.eq.3) then
                    write (ifm,1040) 'HEAVISIDE + CRACK-TIP'
                else
                    write (ifm,1070) 'STATUT          ',statut
                endif
!
!
!
!
901         continue
900     continue
    endif
!
    1040 format (' <CONTACT> ...... ',a25)
    1070 format (' <CONTACT> ...... PARAM. : ',a16,' - VAL. : ',i5)
    1071 format (' <CONTACT> ...... PARAM. : ',a16,' - VAL. : ',e12.5)
    1080 format (' <CONTACT> ... MAILLE : ',a8)
!
    call jedema()
end subroutine
