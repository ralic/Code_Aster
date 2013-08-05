subroutine cfveri(noma, defico, resoco, newgeo, sdappa,&
                  npt, jeux, loca, enti, zone)
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
#include "jeveux.h"
!
#include "asterc/r8prem.h"
#include "asterc/r8vide.h"
#include "asterfort/apcopt.h"
#include "asterfort/apinfi.h"
#include "asterfort/apinfr.h"
#include "asterfort/apvect.h"
#include "asterfort/assert.h"
#include "asterfort/cfcoor.h"
#include "asterfort/cfcorn.h"
#include "asterfort/cfdisi.h"
#include "asterfort/cfdist.h"
#include "asterfort/cfnewj.h"
#include "asterfort/cfnumm.h"
#include "asterfort/cfnumn.h"
#include "asterfort/cftanr.h"
#include "asterfort/infdbg.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/mminfi.h"
#include "asterfort/mminfl.h"
#include "asterfort/mmnorm.h"
#include "asterfort/mmnpoi.h"
#include "asterfort/u2mesk.h"
    character(len=8) :: noma
    character(len=24) :: defico, resoco
    character(len=19) :: newgeo
    character(len=19) :: sdappa
    character(len=24) :: jeux, loca, enti, zone
    integer :: npt
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODE VERIF)
!
! METHODE VERIF POUR LA FORMULATION DISCRETE
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
!
!
!
!
    integer :: ifm, niv
    integer :: typapp, entapp
    integer :: jdecne
    integer :: posmae, nummam, posnoe, posmam, numnoe
    integer :: posnom, numnom
    integer :: izone, ip, iptm, inoe, ipt
    integer :: ndimg, nzoco
    integer :: nbpc, nbpt, npt0
    real(kind=8) :: geomp(3), coorpc(3)
    real(kind=8) :: tau1m(3), tau2m(3)
    real(kind=8) :: tau1(3), tau2(3)
    real(kind=8) :: norm(3), noor
    real(kind=8) :: ksipr1, ksipr2
    real(kind=8) :: r8bid
    real(kind=8) :: jeu, dist
    character(len=8) :: nomnoe, nommam, nomnom, k8bla
    character(len=16) :: nompt, noment
    logical :: lveri
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
    k8bla = ' '
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
    do 10 izone = 1, nzoco
!
! ----- OPTIONS SUR LA ZONE DE CONTACT
!
        nbpt = mminfi(defico,'NBPT' ,izone )
        jdecne = mminfi(defico,'JDECNE',izone )
!
! ----- MODE NON-VERIF: ON SAUTE LES POINTS
!
        lveri = mminfl(defico,'VERIF' ,izone )
        if (.not.lveri) then
            nbpc = mminfi(defico,'NBPC' ,izone )
            ip = ip + nbpc
            goto 25
        endif
!
! ----- BOUCLE SUR LES NOEUDS DE CONTACT
!
        do 20 iptm = 1, nbpt
!
! ------- NOEUD ESCLAVE COURANT
!
            inoe = iptm
            posnoe = jdecne + inoe
!
! ------- INDICE ABSOLU DANS LE MAILLAGE DU NOEUD
!
            call cfnumn(defico, 1, posnoe, numnoe)
!
! ------- NOM DU NOEUD ESCLAVE
!
            call jenuno(jexnum(noma//'.NOMNOE', numnoe), nomnoe)
!
! ------- INFOS APPARIEMENT
!
            call apinfi(sdappa, 'APPARI_TYPE', ip, typapp)
            call apinfi(sdappa, 'APPARI_ENTITE', ip, entapp)
            call apinfr(sdappa, 'APPARI_PROJ_KSI1', ip, ksipr1)
            call apinfr(sdappa, 'APPARI_PROJ_KSI2', ip, ksipr2)
            call apvect(sdappa, 'APPARI_TAU1', ip, tau1m)
            call apvect(sdappa, 'APPARI_TAU2', ip, tau2m)
!
! ------- COORDONNEES DU POINT DE CONTACT
!
            call apcopt(sdappa, ip, coorpc)
!
! ------- NOM DU POINT DE CONTACT
!
            call mmnpoi(noma, k8bla, numnoe, iptm, nompt)
!
! ------- TRAITEMENT DE L'APPARIEMENT
!
            if (typapp .eq. 2) then
!
! --------- MAILLE MAITRE
!
                posmam = entapp
                call cfnumm(defico, 1, posmam, nummam)
!
! --------- NOM DE LA MAILLE MAITRE
!
                call jenuno(jexnum(noma//'.NOMMAI', nummam), nommam)
                noment = nommam
!
! --------- COORDONNEES PROJECTION DU NOEUD ESCLAVE SUR LA MAILLE MAITRE
!
                call cfcoor(noma, defico, newgeo, posmam, ksipr1,&
                            ksipr2, geomp)
!
! --------- RE-DEFINITION BASE TANGENTE SUIVANT OPTIONS
!
                call cftanr(noma, ndimg, defico, resoco, izone,&
                            posnoe, 'MAIL', posmam, nummam, ksipr1,&
                            ksipr2, tau1m, tau2m, tau1, tau2)
!
! --------- CALCUL DE LA NORMALE INTERIEURE
!
                call mmnorm(ndimg, tau1, tau2, norm, noor)
                if (noor .le. r8prem()) then
                    call u2mesk('F', 'CONTACT3_26', 1, nomnoe)
                endif
!
! --------- CALCUL DU JEU
!
                call cfnewj(ndimg, coorpc, geomp, norm, jeu)
!
! --------- CALCUL DU JEU FICTIF DE LA ZONE
!
                call cfdist(defico, 'DISCRETE', izone, posnoe, posmae,&
                            coorpc, dist)
!
! --------- JEU TOTAL
!
                jeu = jeu+dist
            else if (typapp.eq.1) then
!
! --------- NOEUD MAITRE
!
                posnom = entapp
                call cfnumn(defico, 1, posnom, numnom)
!
! --------- NOM DU NOEUD MAITRE
!
                call jenuno(jexnum(noma//'.NOMNOE', numnom), nomnom)
                noment = nomnom
!
! --------- COORDONNNEES DU NOEUD MAITRE
!
                call cfcorn(newgeo, numnom, geomp)
!
! --------- RE-DEFINITION BASE TANGENTE SUIVANT OPTIONS
!
                call cftanr(noma, ndimg, defico, resoco, izone,&
                            posnoe, 'NOEU', posnom, numnom, r8bid,&
                            r8bid, tau1m, tau2m, tau1, tau2)
!
! --------- CALCUL DE LA NORMALE INTERIEURE
!
                call mmnorm(ndimg, tau1, tau2, norm, noor)
                if (noor .le. r8prem()) then
                    call u2mesk('F', 'CONTACT3_26', 1, nomnoe)
                endif
!
! --------- CALCUL DU JEU
!
                call cfnewj(ndimg, coorpc, geomp, norm, jeu)
!
! --------- CALCUL DU JEU FICTIF DE LA ZONE
!
                call cfdist(defico, 'DISCRETE', izone, posnoe, posmae,&
                            coorpc, dist)
!
! --------- JEU TOTAL
!
                jeu = jeu+dist
!
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
! ------- SAUVEGARDE
!
            zr(jjeux+ipt-1) = jeu
            zi(jloca+ipt-1) = numnoe
            zi(jzone+ipt-1) = izone
            zk16(jenti+2*(ipt-1)+1-1) = nompt
            zk16(jenti+2*(ipt-1)+2-1) = noment
!
! ------- LIAISON SUIVANTE
!
            ipt = ipt + 1
            npt0 = npt0+ 1
!
! ------- POINT SUIVANT
!
            ip = ip + 1
!
20      continue
25      continue
10  end do
!
    ASSERT(npt0.eq.npt)
!
    call jedema()
end subroutine
