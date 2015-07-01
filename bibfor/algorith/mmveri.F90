subroutine mmveri(noma, defico, resoco, newgeo, sdappa,&
                  npt, jeux, loca, enti, zone,&
                  instan)
!
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: mickael.abbas at edf.fr
!
    implicit none
#include "asterf_types.h"
#include "jeveux.h"
#include "asterc/r8prem.h"
#include "asterc/r8vide.h"
#include "asterfort/apcopt.h"
#include "asterfort/apinfi.h"
#include "asterfort/apinfr.h"
#include "asterfort/apvect.h"
#include "asterfort/assert.h"
#include "asterfort/cfdisi.h"
#include "asterfort/cfdist.h"
#include "asterfort/cfnumm.h"
#include "asterfort/infdbg.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/mcopco.h"
#include "asterfort/mmelty.h"
#include "asterfort/mminfi.h"
#include "asterfort/mminfl.h"
#include "asterfort/mminfm.h"
#include "asterfort/mmnewj.h"
#include "asterfort/mmnorm.h"
#include "asterfort/mmnpoi.h"
#include "asterfort/mmnumn.h"
#include "asterfort/mmpnoe.h"
#include "asterfort/mmtanr.h"
#include "asterfort/utmess.h"
    character(len=8) :: noma
    character(len=24) :: defico, resoco
    character(len=19) :: newgeo
    character(len=19) :: sdappa
    character(len=24) :: jeux, loca, enti, zone
    integer :: npt
    real(kind=8) :: instan
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODE VERIF)
!
! METHODE VERIF POUR LA FORMULATION CONTINUE
!
! ----------------------------------------------------------------------
!
!
! IN  NOMA   : NOM DU MAILLAGE
! IN  DEFICO : SD POUR LA DEFINITION DE CONTACT
! IN  RESOCO : SD DE TRAITEMENT NUMERIQUE DU CONTACT
! IN  NEWGEO : GEOMETRIE ACTUALISEE
! IN  SDAPPA : NOM DE LA SD APPARIEMENT
! IN  JEUX   : NOM DE LA SD STOCKANT LE JEU
! IN  ENTI   : NOM DE LA SD STOCKANT LES NOMS DES ENTITES APPARIEES
! IN  ZONE   : NOM DE LA SD STOCKANT LA ZONE A LAQUELLE APPARTIENT LE
!              POINT
! IN  LOCA   : NUMERO DU NOEUD POUR LE POINT DE CONTACT (-1 SI LE POINT
!              N'EST PAS UN NOEUD ! )
! IN  NPT    : NOMBRE DE POINTS EN MODE VERIF
! IN  instan : INST value
!
!
!
    integer :: ifm, niv
    integer :: typint, ndexfr
    integer :: typapp, entapp
    integer :: jdecme
    integer :: posmae, nummae, nummam, posnoe, posmam, numnoe
    integer :: izone, imae, ip, iptm, ipt
    integer :: ndimg, nzoco
    integer :: nptm, nbmae, nbpc, nnomae, npt0
    real(kind=8) :: geomp(3), coorpc(3)
    real(kind=8) :: tau1m(3), tau2m(3)
    real(kind=8) :: tau1(3), tau2(3)
    real(kind=8) :: norm(3), noor
    real(kind=8) :: ksipr1, ksipr2
    real(kind=8) :: jeu, dist
    character(len=8) :: nommae, nommam, aliase
    character(len=16) :: nompt, noment
    aster_logical :: lveri, lexfro
    integer :: jjeux, jloca, jenti, jzone
!
! ----------------------------------------------------------------------
!
    call jemarq()
    call infdbg('CONTACT', ifm, niv)
!
! --- INITIALISATIONS
!
    posnoe = 0
    ipt = 1
!
! --- QUELQUES DIMENSIONS
!
    nzoco = cfdisi(defico,'NZOCO' )
    ndimg = cfdisi(defico,'NDIM' )
!
! --- ACCES SD PROVISOIRES
!
    call jeveuo(jeux, 'E', jjeux)
    call jeveuo(loca, 'E', jloca)
    call jeveuo(enti, 'E', jenti)
    call jeveuo(zone, 'E', jzone)
!
! --- BOUCLE SUR LES ZONES
!
    ip = 1
    npt0 = 0
    do izone = 1, nzoco
!
! ----- OPTIONS SUR LA ZONE DE CONTACT
!
        typint = mminfi(defico,'INTEGRATION',izone )
        lveri = mminfl(defico,'VERIF' ,izone )
        nbmae = mminfi(defico,'NBMAE' ,izone )
        jdecme = mminfi(defico,'JDECME',izone )
!
! ----- MODE NON-VERIF: ON SAUTE LES POINTS
!
        if (.not.lveri) then
            nbpc = mminfi(defico,'NBPC' ,izone )
            ip = ip + nbpc
            goto 25
        endif
!
! ----- BOUCLE SUR LES MAILLES ESCLAVES
!
        do imae = 1, nbmae
!
! ------- NUMERO ABSOLU DE LA MAILLE ESCLAVE
!
            posmae = jdecme + imae
            call cfnumm(defico, posmae, nummae)
!
! ------- NOMBRE DE POINTS SUR LA MAILLE ESCLAVE
!
            call mminfm(posmae, defico, 'NPTM', nptm)
!
! ------- INFOS SUR LA MAILLE ESCLAVE
!
            call mmelty(noma, nummae, aliase, nnomae)
            call jenuno(jexnum(noma//'.NOMMAI', nummae), nommae)
            call mminfm(posmae, defico, 'NDEXFR', ndexfr)
            lexfro = (ndexfr.ne.0)
!
! ------- BOUCLE SUR LES POINTS
!
            do iptm = 1, nptm
!
! --------- INFOS APPARIEMENT
!
                call apinfi(sdappa, 'APPARI_TYPE', ip, typapp)
                call apinfi(sdappa, 'APPARI_ENTITE', ip, entapp)
                call apinfr(sdappa, 'APPARI_PROJ_KSI1', ip, ksipr1)
                call apinfr(sdappa, 'APPARI_PROJ_KSI2', ip, ksipr2)
                call apvect(sdappa, 'APPARI_TAU1', ip, tau1m)
                call apvect(sdappa, 'APPARI_TAU2', ip, tau2m)
!
! --------- COORDONNEES DU POINT DE CONTACT
!
                call apcopt(sdappa, ip, coorpc)
!
! --------- APPARIEMENT NODAL INTERDIT !
!
                if (typapp .eq. 1) then
                    ASSERT(.false.)
                endif
!
! --------- INFO SUR LA MAILLE MAITRE
!
                posmam = entapp
                call cfnumm(defico, posmam, nummam)
                call jenuno(jexnum(noma//'.NOMMAI', nummam), nommam)
!
! --------- POSITION DU NOEUD ESCLAVE SI INTEGRATION AUX NOEUDS
!
                call mmpnoe(defico, posmae, aliase, typint, iptm,&
                            posnoe)
!
! --------- NUMERO ABSOLU DU POINT DE CONTACT
!
                call mmnumn(noma, typint, nummae, nnomae, iptm,&
                            numnoe)
!
! --------- COORDONNEES ACTUALISEES DE LA PROJECTION DU PT DE CONTACT
!
                call mcopco(noma, newgeo, ndimg, nummam, ksipr1,&
                            ksipr2, geomp)
!
! --------- RE-DEFINITION BASE TANGENTE SUIVANT OPTIONS
!
                call mmtanr(noma, ndimg, defico, resoco, izone,&
                            lexfro, posnoe, ksipr1, ksipr2, posmam,&
                            nummam, tau1m, tau2m, tau1, tau2)
!
! --------- CALCUL DE LA NORMALE
!
                call mmnorm(ndimg, tau1, tau2, norm, noor)
                if (noor .le. r8prem()) then
                    call jenuno(jexnum(noma//'.NOMMAI', nummam), nommam)
                    call utmess('F', 'CONTACT3_24', sk=nommam)
                endif
!
! --------- CALCUL DU JEU ACTUALISE AU POINT DE CONTACT
!
                call mmnewj(ndimg, coorpc, geomp, norm, jeu)
!
! --------- CALCUL DU JEU FICTIF AU POINT DE CONTACT
!
                call cfdist(defico, 'CONTINUE', izone, posnoe, posmae,&
                            coorpc, dist, instan)
!
! --------- NOM DU POINT DE CONTACT
!
                call mmnpoi(noma, nommae, numnoe, iptm, nompt)
!
! --------- JEU TOTAL + NOM APPARIEMENT
!
                if (typapp .eq. 2) then
                    noment = nommam
                    jeu = jeu+dist
                else if (typapp.eq.-1) then
                    noment = 'EXCLU'
                    jeu = r8vide()
                else if (typapp.eq.-2) then
                    noment = 'EXCLU'
                    jeu = r8vide()
                else if (typapp.eq.-3) then
                    noment = 'EXCLU'
                    jeu = r8vide()
                else
                    ASSERT(.false.)
                endif
!
! --------- SAUVEGARDE
!
                zr(jjeux+ipt-1) = -jeu
                zi(jloca+ipt-1) = numnoe
                zi(jzone+ipt-1) = izone
                zk16(jenti+2*(ipt-1)+1-1) = nompt
                zk16(jenti+2*(ipt-1)+2-1) = noment
!
! --------- LIAISON SUIVANTE
!
                ipt = ipt + 1
                npt0 = npt0+ 1
!
! --------- POINT SUIVANT
!
                ip = ip + 1
!
            end do
        end do
 25     continue
    end do
!
    ASSERT(npt0.eq.npt)
!
    call jedema()
end subroutine
