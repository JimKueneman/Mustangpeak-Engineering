//
//  DetailViewController.h
//  LccThrottle
//
//  Created by Jim Kueneman on 5/19/17.
//  Copyright © 2017 Jim Kueneman. All rights reserved.
//

#import <UIKit/UIKit.h>

@interface DetailViewController : UIViewController

@property (strong, nonatomic) id detailItem;
@property (weak, nonatomic) IBOutlet UILabel *detailDescriptionLabel;

@end

