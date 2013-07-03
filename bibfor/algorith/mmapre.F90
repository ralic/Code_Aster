subroutine mmapre(loptin, noma, numedd, defico, resoco,&
                  sdappa)
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
#include "asterfort/apinfi.h"
#include "asterfort/apinfr.h"
#include "asterfort/apvect.h"
#include "asterfort/armin.h"
#include "asterfort/assert.h"
#include "asterfort/cfdisi.h"
#include "asterfort/cfnumm.h"
#include "asterfort/dismoi.h"
#include "asterfort/infdbg.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/mmapma.h"
#include "asterfort/mmelty.h"
#include "asterfort/mmexcl.h"
#include "asterfort/mmimp1.h"
#include "asterfort/mminfi.h"
#include "asterfort/mminfl.h"
#include "asterfort/mminfm.h"
#include "asterfort/mminfr.h"
#include "asterfort/mmopti.h"
#include "blas/ddot.h"
    logical :: loptin
    character(len=8) :: noma
    character(len=24) :: numedd, defico, resoco
    character(len=19) :: sdappa
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODES CONTINUES - APPARIEMENT)
!
! RECOPIE DE LA SD APPARIEMENT DEDIEE POUR LE CONTACT
!
! ----------------------------------------------------------------------
!
!
! IN  LOPTIN : VAUT .TRUE. SI ACTIVATION DES OPTIONS *_INIT
! IN  NOMA   : NOM DU MAILLAGE
! IN  SDAPPA : NOM DE LA SD APPARIEMENT
! IN  DEFICO : SD POUR LA DEFINITION DE CONTACT
! IN  RESOCO : SD DE TRAITEMENT NUMERIQUE DU CONTACT
!
! ----------------------------------------------------------------------
!
    integer :: ifm, niv
    integer :: nzoco, ndimg
    character(len=24) :: tabfin, crnudd
    integer :: jtabf, jcrnud
    integer :: izone, ip, imae, iptm
    integer :: iptc
    integer :: ntpc, nbpt, nbmae, nptm, neq, nnomae
    real(kind=8) :: tau1m(3), tau2m(3), norm(3)
    real(kind=8) :: ksipr1, ksipr2
    real(kind=8) :: vectpm(3), jeusgn
    real(kind=8) :: seuili, epsint
    real(kind=8) :: armini
    character(len=8) :: aliase, nommam, k8bid
    logical :: lveri
    integer :: ibid, iret
    integer :: jdecme
    integer :: ctcini, typint, typapp, entapp
    integer :: posmae, nummae, posmam, nummam
    logical :: lappar, lgliss, lexfro
    integer :: ndexfr
!
! ----------------------------------------------------------------------
!
    call jemarq()
    call infdbg('CONTACT', ifm, niv)
!
! --- AFFICHAGE
!
    if (niv .ge. 2) then
        write (ifm,*) '<CONTACT> ...... RECOPIE DE L''APPARIEMENT'
    endif
!
! --- INITIALISATIONS
!
    iptc = 1
    ntpc = 0
    call dismoi('F', 'NB_EQUA', numedd, 'NUME_DDL', neq,&
                k8bid, iret)
!
! --- NOUVEL APPARIEMENT
!
    lappar = .true.
!
! --- PARAMETRES
!
    nzoco = cfdisi(defico,'NZOCO' )
    ndimg = cfdisi(defico,'NDIM' )
!
! --- ACCES SD CONTACT
!
    tabfin = resoco(1:14)//'.TABFIN'
    crnudd = resoco(1:14)//'.NUDD'
    call jeveuo(tabfin, 'E', jtabf)
    call jeveuo(crnudd, 'E', jcrnud)
!
! --- TOLERANCE POUR LA DETECTION DU CONTACT INITIAL
!
    armini = armin(noma)
    epsint = 1.d-6*armini
!
! --- BOUCLE SUR LES ZONES
!
    ip = 1
    do 10 izone = 1, nzoco
!
! ----- INFORMATION SUR LA ZONE
!
        jdecme = mminfi(defico,'JDECME',izone )
        nbmae = mminfi(defico,'NBMAE' ,izone )
        lgliss = mminfl(defico,'GLISSIERE_ZONE' ,izone )
        seuili = mminfr(defico,'SEUIL_INIT' ,izone )
        seuili = -abs(seuili)
        ctcini = mminfi(defico,'CONTACT_INIT' ,izone )
        typint = mminfi(defico,'INTEGRATION' ,izone )
!
! ----- MODE VERIF: ON SAUTE LES POINTS
!
        lveri = mminfl(defico,'VERIF' ,izone )
        if (lveri) then
            nbpt = mminfi(defico,'NBPT' ,izone )
            ip = ip + nbpt
            goto 25
        endif
!
! ----- BOUCLE SUR LES MAILLES ESCLAVES
!
        do 20 imae = 1, nbmae
!
! ------- NUMERO ABSOLU DE LA MAILLE ESCLAVE
!
            posmae = jdecme + imae
            call cfnumm(defico, 1, posmae, nummae)
!
! ------- NOMBRE DE POINTS SUR LA MAILLE ESCLAVE
!
            call mminfm(posmae, defico, 'NPTM', nptm)
!
! ------- INFOS SUR LA MAILLE ESCLAVE
!
            call mmelty(noma, nummae, aliase, nnomae, ibid)
!
! ------- NOEUDS EXCLUS PAR SANS_GROUP_NO_FR OU SANS_NOEUD_FR
!
            call mminfm(posmae, defico, 'NDEXFR', ndexfr)
!
! ------- BOUCLE SUR LES POINTS
!
            do 30 iptm = 1, nptm
!
! --------- INFOS APPARIEMENT
!
                call apinfi(sdappa, 'APPARI_TYPE', ip, typapp)
                call apinfi(sdappa, 'APPARI_ENTITE', ip, entapp)
                call apinfr(sdappa, 'APPARI_PROJ_KSI1', ip, ksipr1)
                call apinfr(sdappa, 'APPARI_PROJ_KSI2', ip, ksipr2)
                call apvect(sdappa, 'APPARI_TAU1', ip, tau1m)
                call apvect(sdappa, 'APPARI_TAU2', ip, tau2m)
                call apvect(sdappa, 'APPARI_VECTPM', ip, vectpm)
!
! --------- TRAITEMENT DES NOEUDS EXCLUS
!
                call mmexcl(resoco, typint, iptc, iptm, ndexfr,&
                            typapp, lexfro)
!
! --------- APPARIEMENT NODAL INTERDIT !
!
                if (typapp .eq. 1) then
                    call assert(.false.)
                endif
!
! --------- NUMEROS DE LA MAILLE MAITRE
!
                posmam = entapp
                call cfnumm(defico, 1, posmam, nummam)
!
! --------- SAUVEGARDE APPARIEMENT
!
                call mmapma(noma, defico, resoco, ndimg, izone,&
                            lexfro, typint, aliase, posmae, nummae,&
                            nnomae, posmam, nummam, ksipr1, ksipr2,&
                            tau1m, tau2m, iptm, iptc, norm,&
                            nommam)
!
! --------- JEU SIGNE
!
                jeusgn = ddot(ndimg ,norm ,1 ,vectpm,1 )
!
! --------- TRAITEMENT DES OPTIONS
!
                call mmopti(loptin, resoco, seuili, ctcini, lgliss,&
                            iptc, epsint, jeusgn)
!
! --------- LIAISON DE CONTACT EFFECTIVE
!
                iptc = iptc + 1
                ntpc = ntpc + 1
!
! --------- POINT SUIVANT
!
                ip = ip + 1
!
30          continue
20      continue
25      continue
10  end do
!
! --- NOMBRE TOTAL DE NOEUDS EN CONTACT
!
    zr(jtabf-1+1) = ntpc
    call assert(ntpc.eq.cfdisi(defico, 'NTPC'))
!
! --- INDICATEUR DE REAPPARIEMENT
!
    zl(jcrnud-1+1) = lappar
!
! --- AFFICHAGE
!
    if (niv .ge. 2) then
        call mmimp1(ifm, noma, defico, resoco)
    endif
!
    call jedema()
end subroutine
