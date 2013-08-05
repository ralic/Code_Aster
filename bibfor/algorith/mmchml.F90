subroutine mmchml(noma, defico, resoco, sddisc, sddyna,&
                  numins)
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
!
    implicit none
#include "jeveux.h"
#include "asterfort/alchml.h"
#include "asterfort/assert.h"
#include "asterfort/cfdisi.h"
#include "asterfort/cfmmco.h"
#include "asterfort/cfmmvd.h"
#include "asterfort/detrsd.h"
#include "asterfort/diinst.h"
#include "asterfort/infdbg.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/mmimp3.h"
#include "asterfort/mminfi.h"
#include "asterfort/mminfr.h"
#include "asterfort/ndynlo.h"
#include "asterfort/ndynre.h"
    character(len=8) :: noma
    character(len=24) :: defico, resoco
    character(len=19) :: sddisc, sddyna
    integer :: numins
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODE CONTINUE - CREATION OBJETS - CHAM_ELEM)
!
! CREATION DU CHAM_ELEM CONTENANT LES INFOS DE CONTACT
!
! ----------------------------------------------------------------------
!
!
! IN  NOMA   : NOM DU MAILLAGE
! IN  RESOCO : SD POUR LA RESOLUTION DE CONTACT
! IN  DEFICO : SD POUR LA DEFINITION DE CONTACT
! IN  INST   : PARAMETRES D'INSTANT POUR LA DYNAMIQUE
!
! ----------------------------------------------------------------------
!
    integer :: ncmp
    parameter   (ncmp=28)
    integer :: ztabf
    integer :: iptc, izone, ntpc
    character(len=24) :: jeusup
    integer :: jjsup
    character(len=24) :: tabfin, nosdco
    integer :: jtabf, jnosdc
    integer :: jvalv
    character(len=19) :: ligrcf, chmlcf, crnudd
    integer :: ifm, niv, jcrnud
    real(kind=8) :: instam, instap, deltat
    logical :: ldyna, ltheta, lappar
    real(kind=8) :: theta
    integer :: iform
    real(kind=8) :: coefff
    real(kind=8) :: coefac, coefaf
    integer :: ialgoc, ialgof
    integer :: iresof, iresog
    integer :: iret, ntliel, decal
    integer :: nbgrel, nbliel
    integer :: igr, iel
    character(len=24) :: celd, celv
    integer :: jceld, jcelv, jliel
    integer :: nceld1, nceld2, nceld3
    parameter   (nceld1=4,nceld2=4,nceld3=4)
!
! ----------------------------------------------------------------------
!
    call jemarq()
    call infdbg('CONTACT', ifm, niv)
!
! --- AFFICHAGE
!
    if (niv .ge. 2) then
        write (ifm,*) '<CONTACT> CREATION DU CHAM_ELEM POUR LES'//&
        ' ELEMENTS DE CONTACT'
    endif
!
! --- ACCES OBJETS
!
    jeusup = resoco(1:14)//'.JSUPCO'
    tabfin = resoco(1:14)//'.TABFIN'
    nosdco = resoco(1:14)//'.NOSDCO'
    call jeveuo(nosdco, 'L', jnosdc)
    call jeveuo(jeusup, 'L', jjsup)
    call jeveuo(tabfin, 'L', jtabf)
    crnudd = resoco(1:14)//'.NUDD'
    call jeveuo(crnudd, 'L', jcrnud)
!
    ztabf = cfmmvd('ZTABF')
!
! --- FONCTIONNALITES ACTIVEES
!
    ldyna = ndynlo(sddyna,'DYNAMIQUE')
    ltheta = ndynlo(sddyna,'THETA_METHODE')
    iresof = cfdisi(defico,'ALGO_RESO_FROT')
    iresog = cfdisi(defico,'ALGO_RESO_GEOM')
!
! --- LIGREL DES ELEMENTS TARDIFS DE CONTACT/FROTTEMENT
!
    ligrcf = zk24(jnosdc+2-1)(1:19)
!
! --- CHAM_ELEM POUR ELEMENTS TARDIFS DE CONTACT/FROTTEMENT
!
    chmlcf = resoco(1:14)//'.CHML'
!
! --- INITIALISATIONS
!
    ntpc = nint(zr(jtabf-1+1))
    instam = diinst(sddisc,numins-1)
    instap = diinst(sddisc,numins)
    deltat = instap-instam
    theta = 0.d0
    iform = 0
    if (ldyna) then
        if (ltheta) then
            theta = ndynre(sddyna,'THETA')
            iform = 2
        else
            iform = 1
        endif
    endif
!
! --- DESTRUCTION/CREATION DU CHAM_ELEM SI NECESSAIRE
!
    lappar = zl(jcrnud)
    if (lappar) then
        call detrsd('CHAM_ELEM', chmlcf)
        call alchml(ligrcf, 'RIGI_CONT', 'PCONFR', 'V', chmlcf,&
                    iret, ' ')
        ASSERT(iret.eq.0)
    endif
!
! --- RECUPERATION DU DESCRIPTEUR DU CHAM_ELEM
!
    celd = chmlcf//'.CELD'
    call jeveuo(celd, 'L', jceld)
    nbgrel = zi(jceld-1+2)
!
! --- ACCES AUX VALEURS DU CHAM_ELEM
!
    celv = chmlcf//'.CELV'
    call jeveuo(celv, 'E', jcelv)
!
! --- REMPLISSAGE DU CHAM_ELEM
!
    ntliel = 0
    do 200 igr = 1, nbgrel
!       ADRESSE DANS CELD DES INFORMATIONS DU GREL IGR
        decal = zi(jceld-1+nceld1+igr)
!       NOMBRE D'ELEMENTS DU GREL IGR
        nbliel = zi(jceld-1+decal+1)
!       VERIF TAILLE CHAM_ELEM
        ASSERT(zi(jceld-1+decal+3).eq.ncmp)
!       RECUPERATION DES MAILLES DU GREL IGR
        call jeveuo(jexnum(ligrcf//'.LIEL', igr), 'L', jliel)
        do 300 iel = 1, nbliel
!         MAILLE TARDIVE ZI(JLIEL-1+IEL) < 0
            iptc = -zi(jliel-1+iel)
            izone = nint(zr(jtabf+ztabf*(iptc-1)+13))
            coefff = mminfr(defico,'COEF_COULOMB' ,izone )
            ialgoc = mminfi(defico,'ALGO_CONT' ,izone )
            ialgof = mminfi(defico,'ALGO_FROT' ,izone )
            call cfmmco(defico, resoco, izone, 'COEF_AUGM_CONT', 'L',&
                        coefac)
            call cfmmco(defico, resoco, izone, 'COEF_AUGM_FROT', 'L',&
                        coefaf)
!         ADRESSE DANS CELV DE L'ELEMENT IEL DU GREL IGR
            jvalv = jcelv-1+zi(jceld-1+decal+nceld2+nceld3*(iel-1)+4)
! ------- DONNNES DE PROJECTION
            zr(jvalv-1+1) = zr(jtabf+ztabf*(iptc-1)+3 )
            zr(jvalv-1+2) = zr(jtabf+ztabf*(iptc-1)+4 )
            zr(jvalv-1+3) = zr(jtabf+ztabf*(iptc-1)+5 )
            zr(jvalv-1+4) = zr(jtabf+ztabf*(iptc-1)+6 )
            zr(jvalv-1+5) = zr(jtabf+ztabf*(iptc-1)+7 )
            zr(jvalv-1+6) = zr(jtabf+ztabf*(iptc-1)+8 )
            zr(jvalv-1+7) = zr(jtabf+ztabf*(iptc-1)+9 )
            zr(jvalv-1+8) = zr(jtabf+ztabf*(iptc-1)+10)
            zr(jvalv-1+9) = zr(jtabf+ztabf*(iptc-1)+11)
            zr(jvalv-1+10) = zr(jtabf+ztabf*(iptc-1)+12)
            zr(jvalv-1+11) = zr(jtabf+ztabf*(iptc-1)+14)
! ------- STATUT DE CONTACT
            zr(jvalv-1+12) = zr(jtabf+ztabf*(iptc-1)+22)
! ------- SEUIL DE FROTTEMENT
            zr(jvalv-1+13) = zr(jtabf+ztabf*(iptc-1)+16)
! ------- JEU SUPPLEMENTAIRE
            zr(jvalv-1+14) = zr(jjsup-1+iptc)
! ------- ALGO/COEF DU CONTACT
            zr(jvalv-1+15) = ialgoc
            zr(jvalv-1+16) = coefac
! ------- ALGO/COEF DU FROTTEMENT
            zr(jvalv-1+17) = iresof
            zr(jvalv-1+25) = iresog
            zr(jvalv-1+18) = ialgof
            zr(jvalv-1+19) = coefaf
            zr(jvalv-1+20) = coefff
! ------- EXCLUSION
            zr(jvalv-1+21) = zr(jtabf+ztabf*(iptc-1)+19)
! ------- DYNAMIQUE
            zr(jvalv-1+22) = iform
            zr(jvalv-1+23) = deltat
            zr(jvalv-1+24) = theta
!
            if (niv .ge. 2) then
                call mmimp3(ifm, noma, iptc, jvalv, jtabf)
            endif
300      continue
        ntliel = ntliel + nbliel
200  end do
    ASSERT(ntliel.eq.ntpc)
!
    call jedema()
end subroutine
